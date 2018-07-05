open Batteries;;
open Jhupllib;;

open Core_ast;;
(* open Formula;; *)

open Core_interpreter_utils;;

exception Evaluation_dead_end of string;;

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
let rec wire_in_function graph body enter_node cur_node : unit =
  match body with
  | [] ->
    Hashtbl.add graph cur_node enter_node
  | head :: tail ->
    let b = Unannotated_clause(head) in
    Hashtbl.add graph cur_node b;
    wire_in_function graph tail enter_node b
;;

(*
  Adjacency list representation
  Annotated_Clause -> the nodes/clauses that are directly behind it.
  if a << b then b maps to a since we are traversing backwards

  currently initialize_graph has to do some lookup work to know
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
        wire_in_function graph (List.tl f1_list) (Enter_clause(a, arg, head)) (Unannotated_clause(List.hd (List.rev f1_list)));
        wire_in_function graph (List.tl f2_list) (Enter_clause(b, arg, head)) (Unannotated_clause(List.hd (List.rev f2_list)));

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
let rec lookup lookup_stack (node:annotated_clause) context_stack graph iota: (Core_ast.value * formula) =
  let (cur_var, cur_formula) = Stack.top lookup_stack in
  let a1 = Hashtbl.find graph node in
  print_endline ("\nCurrent lookup variable: " ^ string_of_var cur_var);
  print_endline ("Current node: " ^ string_of_annotated_clause node);
  print_endline ("a1 node: " ^ string_of_annotated_clause a1);
  match a1 with
  | Unannotated_clause(cl) ->
    begin
      let Clause(x, body) = cl in
      if x <> cur_var then
        (* rule 10: Skip *)
        (print_endline "skip";
         lookup lookup_stack a1 context_stack graph iota)
      else
        begin
          match body with
          | Value_body(v) ->
            (* check size of lookup stack to determine if we use rule 1 or 3 *)
            if Stack.length lookup_stack = 1 then
              (* rule 1: value discovery *)
              (print_endline "value discovery";
               if check_formula (substitute_value cur_formula cur_var v) then (v, cur_formula) else failwith "I don't know what to do here. Its a dead end")
            else
              (* rule 3: value discard *)
              (print_endline "value discard";
               let _ = Stack.pop lookup_stack in
               lookup lookup_stack a1 context_stack graph iota)
          | Var_body(v) ->
            (* rule 4: alias *)
            print_endline "alias";
            let _ = Stack.pop lookup_stack in
            Stack.push (v, (substitute_var cur_formula cur_var v)) lookup_stack;
            lookup lookup_stack a1 context_stack graph iota
          | Input ->
            (* rule 2: input. TODO: right now it guesses 5 for all input clauses *)
            begin
              try
                let v = Hashtbl.find iota cur_var in
                if check_formula (substitute_value cur_formula cur_var v) then (v,cur_formula) else failwith "I don't know what to do here. Its a dead end"
              with
              | Not_found ->
                let v = Value_int(5) in
                Hashtbl.add iota x v;
                if check_formula (substitute_value cur_formula x v) then (v,cur_formula) else failwith "I don't know what to do here" (* might make another return type *)
              | _ -> failwith "unhandled exception when looking up iota mapping"
            end
          | Appl_body(xf, xn) ->
            (* wire in dyanmically to set the groundwork to apply rule 9 *)
            print_endline "appl";

            (* find the definition of the function *)
            let temp_stack = Stack.create () in
            Stack.push (xf, true_formula) temp_stack;
            let fcn,_ = lookup temp_stack (Unannotated_clause(cl)) context_stack graph iota in
            begin
              match fcn with
              | Value_function(Function_value(param, Expr(clause_list))) ->
                (* make new nodes *)
                let enter_node = Enter_clause(param, xn, cl) in
                let x' = rv clause_list in
                let exit_node = Exit_clause(cur_var, x', cl) in
                Hashtbl.add graph node exit_node;
                Hashtbl.add graph enter_node (Hashtbl.find graph a1); (* skipping over application node *)

                wire_in_function graph (List.rev clause_list) enter_node exit_node;
                print_graph graph; (* only for debugging *)

                (* call lookup from current node to trigger rule 9 *)
                lookup lookup_stack node context_stack graph iota
              | _ ->
                failwith "fcn wasn't a function"
            end
          | Unary_operation_body(op, var) ->
            let _ = Stack.pop lookup_stack in
            Stack.push (var, true_formula) lookup_stack;
            let v,v_formula = lookup lookup_stack a1 context_stack graph iota in
            begin
              match op with
              | Unary_operator_bool_not ->
                begin
                  match v with
                  | Value_bool(b) -> (Value_bool(not b), v_formula) (* TODO: think it doesn't change *)
                  | _ -> raise @@ Utils.Invariant_failure "non-bool expr with not operator"
                end
              | Unary_operator_bool_coin_flip ->
                failwith "unimplemented coin flip"
            end
          (* | Conditional_body(arg, pattern, Function_value(_, Expr(f1_list)), Function_value(_, Expr(f2_list))) ->

            print_endline "conditional";
            (* Try both branches. First branch is true branch *)
            Stack.push (arg, Binary_formula(Var_formula(arg), Binary_operator_equal_to, Pattern_formula(pattern))) lookup_stack;
            let arg_value_true,formula_1 =
              try
                lookup lookup_stack a1 context_stack graph iota
              with
              | Evaluation_dead_end(msg) ->
                (Empty_value, true_formula)
            in

            begin
              match arg_value_true with
              | Empty_value ->
                ()
              | _ ->
                ()
            end

            Stack.push (arg, Negated_formula(Binary_formula(Var_formula(arg), Binary_operator_equal_to, Pattern_formula(pattern)))) lookup_stack;
            let arg_value_false,formula_2 =
              try
                lookup lookup_stack a1 context_stack graph iota
              with
              | Evaluation_dead_end(msg) ->
                (Empty_value, true_formula)
            in



            (* need to make exit clause now *)
            let new_node = Exit_clause(x, rv fcn_list, cl) in
            (* add exit node to graph, attach rest of function nodes and run again from current node with new binding *)
            Hashtbl.add graph node new_node;
            Hashtbl.add graph new_node (Unannotated_clause(List.hd (List.rev fcn_list)));
            lookup lookup_stack node context_stack graph iota *)
          | _ ->
            failwith "unannotated_clause not implemented yet"
        end
    end
  | Enter_clause(param, arg, (Clause(_, body) as cl)) ->
    if param = cur_var then
      (
        (* rule 7: function enter parameter (local) *)
        print_endline "enter local";
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
    else
      (* rule 8: function enter non-local *)
      begin
        print_endline "non-local";
        match body with
        | Appl_body(xf, _) ->
          Stack.push (xf, true_formula) lookup_stack;
          let cur_context = Stack.pop context_stack in
          begin
            if cur_context <> cl then
              failwith "context did not match"
            else
              lookup lookup_stack a1 context_stack graph iota
          end
        | _ ->
          failwith "enter clause: clause not an appl"
      end
  | Exit_clause(original_program_point, new_program_point, (Clause(_, body) as cl)) ->
    begin
      match body with
      | Appl_body(_, _) ->
        (* rule 9: function exit. Some premises were verified in the appl case of lookup. *)
        let _ = Stack.pop lookup_stack in
        Stack.push (new_program_point, (substitute_var cur_formula original_program_point new_program_point)) lookup_stack;
        Stack.push cl context_stack;
        lookup lookup_stack a1 context_stack graph iota
      | Conditional_body(_,_,_,_) ->
        (* implement rules 15 and 16 here *)
        failwith "went into fcn without appl"
      | _ ->
        failwith "exit clause context is no appl or cond"
    end
  | Start_clause ->
    raise (Evaluation_dead_end "reached start of the program")
  | End_clause ->
    print_endline "end";
    lookup lookup_stack (Hashtbl.find graph node) context_stack graph iota
;;

(* record rules on page 37 *)


let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t * formula =
  let context_stack:(clause) Stack.t = Stack.create () in
  let lookup_stack:(var * formula) Stack.t = Stack.create () in
  let iota:(Core_ast.var, Core_ast.value) Hashtbl.t = Hashtbl.create 10 in

  let rx = rv cls in
  (* start lookup with the last program point *)
  Stack.push (rx, true_formula) lookup_stack;
  (* Stack.push (x, true_formula) lookup_stack; *)

  (* make graph *)
  let graph:(annotated_clause, annotated_clause) Hashtbl.t = initialize_graph (End_clause) (List.rev cls) (Hashtbl.create 10) in

  (* this is to fit the return value the toploop expects *)
  let env = Core_interpreter.Environment.create 10 in

  (* do lookup *)
  let v,formula = lookup lookup_stack End_clause context_stack graph iota in
  (* Core_interpreter.Environment.add env x v;
     x, env *)
  Core_interpreter.Environment.add env rx v;
  rx, env, formula
;;
