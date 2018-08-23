open Batteries;;
open Jhupllib;;

open Core_ast;;
(* open Core_ast_pp;; *)
(* open Pp_utils;; *)

(* open Formula;; *)

open Core_interpreter_utils;;

exception Evaluation_dead_end of string;;

(* module VS = Set.Make(Value);; *)

let evaluation_counter = ref 0;;

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

(*
  called when appl or conditional is encountered.
  Creates function block and wires it into the right place
*)
let rec wire_in_function graph body a : unit =
  match body with
  | [] ->
    ()
  | head :: tail ->
    let b = Unannotated_clause(head) in
    Hashtbl.add graph b a;
    wire_in_function graph tail b
;;

(*
  Adjacency list representation
  Annotated_Clause -> the nodes/clauses that are directly behind it.
  if a << b then b maps to a since we are traversing backwards

  Expects clause list to be backwards
*)
let rec initialize_graph (prev: annotated_clause) (cls: clause list) (graph: (annotated_clause, annotated_clause) Hashtbl.t) :
  (annotated_clause, annotated_clause) Hashtbl.t =
  match cls with
  | [] ->
    Hashtbl.add graph prev Start_clause;
    graph
  | Clause(_, body) as head :: tail ->
    begin
      match body with
      | Conditional_body(arg, _, Function_value(a, Expr(f1_list)), Function_value(b, Expr(f2_list))) ->
        wire_in_function graph f1_list (Enter_clause(a, arg, head));
        wire_in_function graph f2_list (Enter_clause(b, arg, head));

        Hashtbl.add graph prev (Unannotated_clause(head));
        initialize_graph (Unannotated_clause(head)) tail graph;
      | _ ->
        Hashtbl.add graph prev (Unannotated_clause(head));
        initialize_graph (Unannotated_clause(head)) tail graph
    end
;;

(*
  lookup function
*)
let rec lookup lookup_stack (node:annotated_clause) context_stack graph iota: (Core_ast.value * formula * input_mapping) =
  let (cur_var, cur_formula) = Stack.top lookup_stack in
  let a1 =
    try
      Hashtbl.find graph node
    with
    | Not_found -> failwith "not in graph"
  in
  print_endline ("\nCurrent lookup variable: " ^ string_of_var cur_var);
  print_endline ("Current node: " ^ string_of_annotated_clause node);
  print_endline ("a1 node: " ^ string_of_annotated_clause a1);
  print_stack lookup_stack;
  print_endline ("current formula: " ^ string_of_formula cur_formula);
  (* print_graph graph; *)
  match a1 with
  | Unannotated_clause(cl) ->
    begin
      let Clause(x, body) = cl in
      if x <> cur_var then
        (* rule 10: Skip *)
        (
          (* print_endline "skip"; *)
          lookup lookup_stack a1 context_stack graph iota
        )
      else
        begin
          match body with
          | Value_body(v) ->
            (* check size of lookup stack to determine if we use rule 1 or 3 *)
            if Stack.length lookup_stack = 1 then
              (* rule 1: value discovery *)
              (
                (* print_endline "value discovery"; *)
                (* print_endline (string_of_formula cur_formula); *)
                if check_formula (substitute_value cur_formula cur_var v) then
                  (v, cur_formula, iota)
                else
                  raise (Evaluation_dead_end "formula unsatisfied")
              )
            else
              (* rule 3: value discard *)
              (
                (* print_endline "value discard"; *)
                print_stack lookup_stack;
                let _ = Stack.pop lookup_stack in
                lookup lookup_stack a1 context_stack graph iota
              )
          | Var_body(v) ->
            (* rule 4: alias *)
            (* print_endline "alias"; *)
            let _ = Stack.pop lookup_stack in
            Stack.push (v, (substitute_var cur_formula cur_var v)) lookup_stack;
            lookup lookup_stack a1 context_stack graph iota
          | Input ->
            (* rule 2: input. TODO: right now it guesses 0 for all input clauses *)
            (* print_endline "input"; *)
            begin
              try
                let v = Iota.find iota cur_var in
                if check_formula (substitute_value cur_formula cur_var v) then
                  (v, cur_formula, iota)
                else
                  raise (Evaluation_dead_end "formula unsatisfied")
              with
              | Not_found ->
                let v = Value_int(0) in
                Iota.add iota x v;
                if check_formula (substitute_value cur_formula x v) then
                  (v, cur_formula, iota)
                else
                  raise (Evaluation_dead_end "formula unsatisfied")
                  (* | _ -> failwith "unhandled exception when looking up iota mapping" *)
            end
          | Appl_body(xf, xn) ->
            (* wire in dyanmically to set the groundwork to apply rule 9 *)
            (* print_endline "appl"; *)

            (* find the definition of the function *)
            let temp_stack = Stack.create () in
            Stack.push (xf, true_formula) temp_stack;
            let fcn,_,_ = lookup temp_stack (Unannotated_clause(cl)) context_stack graph iota in
            begin
              match fcn with
              | Value_function(Function_value(param, Expr(clause_list))) ->
                (* make new nodes *)
                let enter_node = Enter_clause(param, xn, cl) in
                let x' = rv clause_list in
                let exit_node = Exit_clause(cur_var, x', cl) in
                Hashtbl.add graph node exit_node;
                Hashtbl.add graph exit_node (Unannotated_clause(List.hd (List.rev clause_list)));
                Hashtbl.add graph enter_node (Hashtbl.find graph a1); (* skipping over application node *)

                wire_in_function graph clause_list enter_node;
                (* print_graph graph; (* only for debugging *) *)

                (* call lookup from current node to trigger rule 9 *)
                lookup lookup_stack node context_stack graph iota
              | _ ->
                failwith "fcn wasn't a function"
            end
          | Conditional_body(arg, pattern, Function_value(a, Expr(f1_list)), Function_value(b, Expr(f2_list))) ->
            (* print_endline "conditional"; *)
            (* No non-determinism here. Execute the pattern match first to determine which function to wire in *)
            let new_stack = Stack.create () in (* to prevent value discard from being called *)
            Stack.push (arg, true_formula) new_stack;
            (* isn't the formula that is returned by the next call really important?
                feel like i need to take this formula and boolean and it with the formula that results from the last line of this match
            *)
            let arg_value,_,_ = lookup new_stack a1 context_stack graph iota in
            let fcn_list,param =
              (
                if (matches arg_value pattern) then
                  f1_list,a
                else
                  f2_list,b
              )
            in
            (* need to make exit clause now *)
            let exit_node = Exit_clause(x, rv fcn_list, cl) in
            (* add exit node to graph, attach rest of function nodes and run again from current node with new binding *)
            Hashtbl.add graph node exit_node;
            Hashtbl.add graph exit_node (Unannotated_clause(List.hd (List.rev fcn_list)));
            Hashtbl.add graph (Enter_clause(param, arg, cl)) (Hashtbl.find graph a1);
            (* print_endline "concluding conditional";
               print_graph graph; *)
            lookup lookup_stack node context_stack graph iota
          | Unary_operation_body(op, var) ->
            (* print_endline "unary"; *)
            let _ = Stack.pop lookup_stack in
            Stack.push (var, true_formula) lookup_stack;
            let v,v_formula,_ = lookup lookup_stack a1 context_stack graph iota in
            begin
              match op with
              | Unary_operator_bool_not ->
                begin
                  match v with
                  | Value_bool(b) -> (Value_bool(not b), v_formula, iota) (* TODO: think it doesn't change *)
                  | _ -> raise @@ Utils.Invariant_failure "non-bool expr with not operator"
                end
              | Unary_operator_bool_coin_flip ->
                failwith "unimplemented coin flip"
            end
          | Binary_operation_body(v1, op, v2) ->
            (* print_endline "binary"; *)
            (* implementing left first *)
            (* assuming that the lookup stack never has anything else on it *)
            (* if (Stack.length lookup_stack > 1) then
               print_endline "lookup_stack has more stuff on it (binary)"; *)
            (* else
               ( *)
            let _ = Stack.pop lookup_stack in
            if (Stack.length lookup_stack != 0) then
              failwith "ahh"
            else
              (
                let left_stack = Stack.create () in
                Stack.push (v1, true_formula) left_stack;
                (* TODO: check left_formula *)
                let left_value,_,_ = lookup left_stack a1 context_stack graph iota in
                let new_formula = substitute_formula cur_formula cur_var (Binary_formula(Value_formula(left_value), op, Var_formula(v2))) in
                (* pop off v1 *)
                (* let _ = Stack.pop lookup_stack in *)
                let right_stack = Stack.create () in
                Stack.push (v2, new_formula) right_stack;
                let right_value,_,_ = lookup right_stack a1 context_stack graph iota in
                begin
                  match op with
                  | Binary_operator_plus ->
                    begin
                      match left_value, right_value with
                      | Value_int(i1), Value_int(i2) -> Value_int(i1 + i2), new_formula, iota
                      | _ -> failwith "tried to add non-int"
                    end
                  | Binary_operator_int_minus ->
                    begin
                      match left_value, right_value with
                      | Value_int(i1), Value_int(i2) -> Value_int(i1 - i2), new_formula, iota
                      | _ -> failwith "tried to subtract non-int"
                    end
                  | Binary_operator_int_less_than ->
                    begin
                      match left_value, right_value with
                      | Value_int(i1), Value_int(i2) -> Value_bool(i1 < i2), new_formula, iota
                      | _ -> failwith "tried to less than non-int"
                    end
                  | Binary_operator_int_less_than_or_equal_to ->
                    begin
                      match left_value, right_value with
                      | Value_int(i1), Value_int(i2) -> Value_bool(i1 <= i2), new_formula, iota
                      | _ -> failwith "tried to less than or equal to non-int"
                    end
                  | Binary_operator_equal_to ->
                    begin
                      match left_value, right_value with
                      | Value_int(i1), Value_int(i2) -> Value_bool(i1 = i2), new_formula, iota
                      | Value_bool(b1), Value_bool(b2) -> Value_bool(b1 = b2), new_formula, iota
                      | _ -> failwith "tried to equal two different types"
                    end
                  | Binary_operator_bool_and ->
                    begin
                      match left_value, right_value with
                      | Value_bool(b1), Value_bool(b2) -> Value_bool(b1 && b2), new_formula, iota
                      | _ -> failwith "tried to and non-booleans"
                    end
                  | Binary_operator_bool_or ->
                    begin
                      match left_value, right_value with
                      | Value_bool(b1), Value_bool(b2) -> Value_bool(b1 || b2), new_formula, iota
                      | _ -> failwith "tried to and non-booleans"
                    end
                  | Binary_operator_index -> failwith "index not done yet"
                  | Binary_operator_tilde -> failwith "todo core interpreter 2"
                end
              )
          | _ ->
            failwith "unannotated_clause not implemented yet"
        end
    end
  | Enter_clause(param, arg, (Clause(_, body) as cl)) ->
    begin
      match body with
      | Appl_body(xf, _) ->
        if param = cur_var then
          (
            if Stack.is_empty context_stack then
              (
                (* rule 9: function enter parameter empty stack*)
                (* print_endline "empty enter local"; *)
                let temp_stack = Stack.create () in
                Stack.push (xf, true_formula) temp_stack;
                (* this will crash if function is not in graph *)
                let _ = lookup temp_stack a1 (Stack.create ()) graph iota in
                let _ = Stack.pop lookup_stack in
                Stack.push (arg, (substitute_var cur_formula param arg)) lookup_stack;
                lookup lookup_stack a1 context_stack graph iota
              )
            else
              (
                (* rule 7: function enter parameter (local) *)
                (* print_endline "enter local"; *)
                let _ = Stack.pop lookup_stack in
                Stack.push (arg, (substitute_var cur_formula param arg)) lookup_stack;
                let cur_context = Stack.pop context_stack in
                begin
                  if cur_context <> cl then
                    failwith "context did not match"
                  else
                    lookup lookup_stack a1 context_stack graph iota
                end
              )
          )
        else
          (
            if Stack.is_empty context_stack then
              (
                (* rule 10: function enter empty non-local *)
                (* print_endline "empty enter non-local"; *)
                let temp_stack = Stack.create () in
                Stack.push (xf, true_formula) temp_stack;
                (* this will crash if function is not in graph *)
                let _ = lookup temp_stack a1 (Stack.create ()) graph iota in
                Stack.push (xf, true_formula) lookup_stack;
                lookup lookup_stack a1 context_stack graph iota
              )
            else
              (
                (* rule 8: function enter non-local *)
                (* print_endline "non-local"; *)
                Stack.push (xf, true_formula) lookup_stack;
                let cur_context = Stack.pop context_stack in
                if cur_context <> cl then
                  failwith "context did not match"
                else
                  lookup lookup_stack a1 context_stack graph iota

              )
          )
      | Conditional_body(a, p, Function_value(_, Expr(f1_list)), Function_value(_, Expr(f2_list))) ->
        (* if cur_var <> param then
           (
            (* rule 14. I think this should never happen either just because of the way we wire in conditionals *)
            print_endline "14";
            let _ = Stack.pop lookup_stack in
            lookup lookup_stack a1 context_stack graph iota
           )
           else  *)
        if Stack.is_empty context_stack then
          (
            (* we started inside a conditional *)
            (* figure out which branch I'm in *)
            (* print_endline "15"; *)
            let Clause(xf1, _) = List.hd f1_list in
            let Clause(xf2, _) = List.hd f2_list in
            let cur_node_var =
              begin
                match node with
                | Unannotated_clause(Clause(cnv, _)) -> cnv
                | _ -> failwith "current node is not unannotated"
              end
            in
            let _, new_formula =
              (
                let pattern_formula = Binary_formula(Var_formula(a), Binary_operator_tilde, Pattern_formula(p)) in
                if xf1 = cur_node_var then
                  xf1, Binary_formula(pattern_formula, Binary_operator_equal_to, Value_formula(Value_bool(true)))
                else
                  xf2, Binary_formula(pattern_formula, Binary_operator_equal_to, Value_formula(Value_bool(false)))
              )
            in
            let _ = Stack.pop lookup_stack in
            Stack.push (arg, Binary_formula(new_formula, Binary_operator_bool_and, (substitute_var cur_formula cur_var arg))) lookup_stack;

            lookup lookup_stack (find_by_value graph (find_clause_by_var graph arg)) context_stack graph iota
          )
        else
          (
            (* print_endline "16"; *)
            (* just business as usual *)
            let _ = Stack.pop lookup_stack in
            Stack.push (arg, (substitute_var cur_formula cur_var arg)) lookup_stack;

            (* print_endline "here"; *)
            let cur_context = Stack.pop context_stack in
            if cur_context <> cl then
              failwith "context did not match"
            else
              lookup lookup_stack a1 context_stack graph iota
          )
      | _ ->
        failwith "enter clause context not conditional or appl"
    end
  | Exit_clause(original_program_point, new_program_point, (Clause(_, body) as cl)) ->
    begin
      match body with
      | Appl_body(_, _)
      | Conditional_body(_,_,_,_) ->
        (* rule 9, 15, 16: function exit, conditional bottom true and false. Premises were verified in the previous match case of lookup. *)
        let _ = Stack.pop lookup_stack in
        Stack.push (new_program_point, (substitute_var cur_formula original_program_point new_program_point)) lookup_stack;
        Stack.push cl context_stack;
        lookup lookup_stack a1 context_stack graph iota
      | _ ->
        failwith "exit clause context is not appl or cond"
    end
  | Start_clause ->
    raise (Evaluation_dead_end "reached start of the program")
  | End_clause
  | Junk_clause ->
    (* print_endline "end"; *)
    lookup lookup_stack a1 context_stack graph iota
;;

let rec eval_helper lookup_stack starting_node context_stack graph iota starting_program_point: Core_ast.value * formula * input_mapping =
  if (!evaluation_counter > 100) then
    failwith "Evaluation max reached"
  else
    try
      print_endline ("evaluation number: " ^ string_of_int !evaluation_counter);
      let lookup_stack = Stack.create () in
      Stack.push (starting_program_point, true_formula) lookup_stack;
      lookup lookup_stack starting_node context_stack graph iota
    with
    | Evaluation_dead_end(_) ->
      evaluation_counter := !evaluation_counter +1;
      eval_helper lookup_stack starting_node context_stack graph (successor iota) starting_program_point
;;


let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t * formula * input_mapping =
  (* let f1 = Binary_formula(Value_formula(Value_int(1)), Binary_operator_tilde, Pattern_formula(Int_pattern)) in
     let f2 = Binary_formula(Value_formula(Value_int(2)), Binary_operator_plus, f1) in
     let temp = evaluate_patterns f2 in
     print_endline "hi";
     print_endline (string_of_formula temp); *)

  let context_stack:(clause) Stack.t = Stack.create () in
  let lookup_stack:(var * formula) Stack.t = Stack.create () in
  let iota = Iota.create 10 in

  (* remove last clause from program and store program point specified by it *)
  let clause_list, starting_program_point =
    match (List.rev cls) with
    | [] -> failwith "empty program"
    | Clause(_, x) :: tail ->
      begin
        match x with
        | Var_body(v) ->
          tail, v
        | _ ->
          failwith "last line was not an alias"
      end
  in

  (* make graph and add CFG edges by looking up last program point *)
  let graph:(annotated_clause, annotated_clause) Hashtbl.t = initialize_graph (End_clause) clause_list (Hashtbl.create 10) in
  let rx = rv (List.rev clause_list) in
  Stack.push (rx, true_formula) lookup_stack;
  print_graph graph;
  let _ = lookup lookup_stack End_clause context_stack graph iota in

  (* now need to find the node that starts with program_point *)
  let starting_node, new_graph = find_starting_node graph starting_program_point in
  let graph = new_graph in

  print_endline "\nstatic CFG construction complete";
  print_graph graph;
  (* print_endline ("starting_node: " ^ (string_of_annotated_clause starting_node));
     print_endline ("a1: " ^ (string_of_annotated_clause (Hashtbl.find graph starting_node))); *)


  (* do lookup *)
  let lookup_stack = Stack.create () in
  Stack.push (starting_program_point, true_formula) lookup_stack;
  let v,formula,new_iota = eval_helper lookup_stack starting_node context_stack graph iota starting_program_point in

  print_endline "iota:";
  print_endline (string_of_input_mapping new_iota);

  (* this is to fit the return value the toploop expects *)
  let env = Core_interpreter.Environment.create 10 in
  Core_interpreter.Environment.add env rx v;
  rx, env, formula, new_iota
;;
