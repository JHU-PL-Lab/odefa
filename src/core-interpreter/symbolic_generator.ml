open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_interpreter_utils;;

open Unix;;
open Yojson.Basic.Util;;

type program_state = Done of (value * formula list) |
                     Working of ((var * (clause Stack.t)) Stack.t * annotated_clause * clause Stack.t * (annotated_clause,annotated_clause) Hashtbl.t * formula list) |
                     Double_working of (program_state * program_state);;

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

let rv_of_function (fcn:value) : var =
  match fcn with
  | Value_function(Function_value(_,Expr(cls))) ->
    rv cls
  | _ ->
    failwith "Not a function - rv of function"
;;

let script formulas_list =
  execv "/usr/bin/python" [| "python";"/home/theodore/research/odefa/src/core-interpreter/test.py";formulas_list|]
;;

(* note that when we recurse into a function, we add the outermost level declarations first - so when we traverse the graph we go into the function first *)
let rec make_map clauses graph : (var,clause_body) Hashtbl.t =
  match clauses with
  | [] -> graph
  | Clause(var, body) :: tail ->
    match body with
    | Value_body(Value_function(Function_value(_, Expr(fcn_body)))) ->
      Hashtbl.add graph var body;
      (* recurse into function body *)
      make_map tail (make_map fcn_body graph)
    | Conditional_body(_,_,Function_value(_, Expr(f1)),Function_value(_, Expr(f2))) ->
      Hashtbl.add graph var body;
      (* recurse into function bodies *)
      make_map tail (make_map f2 (make_map f1 graph))
    | _ ->
      Hashtbl.add graph var body;
      make_map tail graph
;;

let get_annotated_clause_from_json (edge:Yojson.Basic.json list) map : annotated_clause =
  let node_type = to_string (List.hd edge) in
  if node_type = "Unannotated_clause" then
    (* the first entry in this list is Unannotated_clause *)
    (* edge *)
    (* [
       "Unannotated_clause",
       [
        "Abs_clause",
        [ "Abs_var", [ "Ident", "x" ] ],
        [ "Abs_value_body", [ "Abs_value_int" ] ]
       ]
       ] *)
    let node_outer_list = to_list (List.nth edge 1) in
    (* first entry is Abs_clause *)
    let node_list = to_list (List.nth node_outer_list 1) in
    (* Abs_var is first entry *)
    let ident_list = to_list (List.nth node_list 1) in
    (* Ident is first entry and get to x *)
    let ident = to_string (List.nth ident_list 1) in
    let var = Var(Ident(ident), None) in
    (* get the node clause body from the map *)
    let node_to_add =
      try
        Hashtbl.find map var
      with
      | Not_found -> print_endline ("ident: " ^ ident); failwith "Var not found"
    in
    Unannotated_clause(Clause(var, node_to_add))
  else if node_type = "Start_clause" then
    let var_ident = List.nth (List.nth (List.nth edge 1 |> to_list) 1 |> to_list) 1 |> to_string in
    Start_clause(Core_ast.Var(Ident(var_ident),None))
  else if node_type = "End_clause" then
    let var_ident = List.nth (List.nth (List.nth edge 1 |> to_list) 1 |> to_list) 1 |> to_string in
    End_clause(Core_ast.Var(Ident(var_ident),None))
  else if node_type = "Enter_clause" then
    let param = List.nth (List.nth (List.nth edge 1 |> to_list) 1 |> to_list) 1 |> to_string in
    let arg = List.nth (List.nth (List.nth edge 2 |> to_list) 1 |> to_list) 1 |> to_string in
    let context_var = Var(Ident(List.nth (List.nth (List.nth (List.nth edge 3 |> to_list) 1 |> to_list) 1 |> to_list) 1 |> to_string), None) in
    let context_clause =
      try
        Hashtbl.find map context_var
      with
      | Not_found ->
        failwith "Context clause not found in map"
    in
    Enter_clause(Core_ast.Var(Ident(param),None), Core_ast.Var(Ident(arg),None), Clause(context_var, context_clause))
  else if node_type = "Exit_clause" then
    let old_program_pt = List.nth (List.nth (List.nth edge 1 |> to_list) 1 |> to_list) 1 |> to_string in
    let new_program_pt = List.nth (List.nth (List.nth edge 2 |> to_list) 1 |> to_list) 1 |> to_string in
    let context_var = Var(Ident(List.nth (List.nth (List.nth (List.nth edge 3 |> to_list) 1 |> to_list) 1 |> to_list) 1 |> to_string), None) in
    let context_clause =
      try
        Hashtbl.find map context_var
      with
      | Not_found ->
        failwith "Context clause not found in map"
    in
    Exit_clause(Core_ast.Var(Ident(old_program_pt),None), Core_ast.Var(Ident(new_program_pt),None), Clause(context_var, context_clause))
  else
    failwith "Unrecognized node type in json ddpa graph"
;;

let rec initialize_graph (map: (var,clause_body) Hashtbl.t) (elements:Yojson.Basic.json list) graph: (annotated_clause,annotated_clause) Hashtbl.t =
  match elements with
  | [] -> graph
  | head :: tail ->
    let node1 = get_annotated_clause_from_json (to_list (List.nth (to_list head) 1)) map in
    let node2 = get_annotated_clause_from_json (to_list (List.nth (to_list head) 2)) map in
    Hashtbl.add graph node2 node1;
    initialize_graph map tail graph
;;

let mayBeTop (context_stack:Core_ast.clause Stack.t) (context:Core_ast.clause) : bool =
  if Stack.is_empty context_stack then
    true
  else if Stack.top context_stack = context then
    true
  else
    false
;;

(*
  lookup function
*)
let rec lookup (lookup_stack:(var * (clause Stack.t)) Stack.t) (node:annotated_clause) context_stack graph phi: program_state =
  let (cur_var, _) = Stack.top lookup_stack in
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
  print_context_stack context_stack;
  print_phi phi;
  match a1 with
  | Unannotated_clause(cl) ->
    let Clause(x, body) = cl in
    if x <> cur_var then
      (
        lookup lookup_stack a1 context_stack graph phi
      )
    else
      begin
        match body with
        | Value_body(v) ->
          if Stack.length lookup_stack > 1 then
            let _ = Stack.pop lookup_stack in
            lookup lookup_stack node context_stack graph phi
          else
            Done(v, phi@[Binary_formula(Var_formula(x, (Stack.copy context_stack)), Binary_operator_equal_to, Value_formula(v))])
        | Var_body(var) ->
          let _ = Stack.pop lookup_stack in
          Stack.push (var, (Stack.copy context_stack)) lookup_stack;
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var, (Stack.copy context_stack)),Binary_operator_equal_to, Var_formula(var, (Stack.copy context_stack)))])
        | Input ->
          (* solve(Or(x <= 1, 1 <= x)) *)
          let negCover = Binary_formula(Var_formula(x, (Stack.copy context_stack)),Binary_operator_int_less_than_or_equal_to, Value_formula(Value_int(1))) in
          let posCover = Binary_formula(Value_formula(Value_int(1)), Binary_operator_int_less_than_or_equal_to, Var_formula(x, (Stack.copy context_stack))) in
          let existence_formula = Binary_formula(negCover, Binary_operator_bool_or, posCover) in
          Done(Value_int(0),phi@[existence_formula]) (* pretty sure the value here is garbage - well what if we depend on it later? *)
        | Appl_body(_,_) ->
          failwith "appl body encountered - this should never happen I think"
        | Unary_operation_body(op, v1) ->
          let _ = Stack.pop lookup_stack in
          Stack.push (v1, (Stack.copy context_stack)) lookup_stack;
          let q1 = Queue.create () in
          Queue.add (Working(lookup_stack, a1, context_stack, graph, phi)) q1;
          (* let v2, phi2 = eval_helper q1 false in *)
          let v2, phi2 = eval_helper q1 true in
          begin
            match op with
            | Unary_operator_bool_not ->
              begin
                match v2 with
                | Value_bool(b) -> Done(Value_bool(not b), phi@phi2@[Binary_formula(Var_formula(cur_var, (Stack.copy context_stack)),Binary_operator_equal_to, Negated_formula(Var_formula(v1, (Stack.copy context_stack))))])
                | _ -> raise @@ Utils.Invariant_failure "non-bool expr with not operator"
              end
            | Unary_operator_bool_coin_flip ->
              failwith "unimplemented coin flip"
          end
        | Binary_operation_body(v1, op, v2) ->
          let _ = Stack.pop lookup_stack in
          let original_context_stack = Stack.copy context_stack in
          let lookup_stack1 = Stack.copy lookup_stack in
          Stack.push (v1, (Stack.copy context_stack)) lookup_stack1;
          let context_stack1 = Stack.copy context_stack in
          let lookup_stack2 = Stack.copy lookup_stack in
          Stack.push (v2, (Stack.copy context_stack)) lookup_stack2;
          let context_stack2 = Stack.copy context_stack in

          (* spawning new eval process b/c want it to terminate fully *)
          let q1 = Queue.create () in
          Queue.add (Working(lookup_stack1, a1, context_stack1, graph, phi)) q1;
          print_endline "--------------------------------------------------------------Starting left lookup--------------------------------------------------------------";
          print_endline (string_of_var v1);
          let left_value,phi1 = eval_helper q1 true in
          print_endline "--------------------------------------------------------------Left lookup DONE--------------------------------------------------------------";
          print_endline (string_of_var v1);

          let q2 = Queue.create () in
          Queue.add (Working(lookup_stack2, a1, context_stack2, graph, phi)) q2;
          print_endline "--------------------------------------------------------------Starting r lookup--------------------------------------------------------------";
          print_endline (string_of_var v2);
          let right_value,phi2 = eval_helper q2 true in
          print_context_stack context_stack2;
          print_endline "--------------------------------------------------------------r lookup DONE--------------------------------------------------------------";
          print_endline (string_of_var v2);
          let phi = phi@phi1@phi2 in
          begin
            match op with
            | Binary_operator_plus ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  Done(Value_int(i1 + i2), phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                               Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_plus, Var_formula(v2, (Stack.copy original_context_stack))))])
                | _ -> failwith "tried to add non-int"
              end
            | Binary_operator_int_minus ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  Done(Value_int(i1 - i2), phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                               Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_int_minus, Var_formula(v2, (Stack.copy original_context_stack))))])
                | _ -> failwith "tried to subtract non-int"
              end
            | Binary_operator_int_less_than ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  Done(Value_bool(i1 < i2), phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                                Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_int_less_than, Var_formula(v2, (Stack.copy original_context_stack))))])
                | _ -> failwith "tried to less than non-int"
              end
            | Binary_operator_int_less_than_or_equal_to ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  Done(Value_bool(i1 <= i2), phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                                 Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_int_less_than_or_equal_to, Var_formula(v2, (Stack.copy original_context_stack))))])
                | _ -> failwith "tried to less or equal than non-int"
              end
            | Binary_operator_equal_to ->
              let new_formula = phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                    Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_equal_to, Var_formula(v2, (Stack.copy original_context_stack))))] in
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  Done(Value_bool(i1 = i2), new_formula)
                | Value_bool(b1), Value_bool(b2) -> Done(Value_bool(b1 = b2), new_formula)
                | _ -> failwith "tried to equal two different types"
              end
            | Binary_operator_bool_and ->
              begin
                match left_value, right_value with
                | Value_bool(b1), Value_bool(b2) ->
                  Done(Value_bool(b1 && b2), phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),
                                                                 Binary_operator_equal_to, Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_bool_and, Var_formula(v2, (Stack.copy original_context_stack))))])
                | _ -> failwith "tried to and non-booleans"
              end
            | Binary_operator_bool_or ->
              begin
                match left_value, right_value with
                | Value_bool(b1), Value_bool(b2) ->
                  Done(Value_bool(b1 || b2), phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                                 Binary_formula(Var_formula(v1, (Stack.copy original_context_stack)), Binary_operator_bool_or, Var_formula(v2, (Stack.copy original_context_stack))))])
                | _ -> failwith "tried to or non-booleans"
              end
            | Binary_operator_index -> failwith "index not done yet"
            | Binary_operator_tilde -> failwith "tilde binop"
          end
        | _ ->
          failwith "not implemented yet"
      end
  | Enter_clause(param, arg, (Clause(_, body) as cl)) ->
    begin
      match body with
      | Appl_body(xf, xn) ->
        if cur_var = param then
          if Stack.is_empty context_stack then
            failwith "haven't dealt with this"
          else
            let original_context_stack = Stack.copy context_stack in
            let cur_context = Stack.pop context_stack in
            if cur_context = cl then
              (* let new_stack = Stack.create () in
                 Stack.push (xf, "placeholder") new_stack;
                 let new_context_stack = Stack.copy context_stack in
                 let _ =
                 try
                  lookup new_stack node new_context_stack graph phi
                 with
                 | Not_found -> failwith ("function " ^ (string_of_var xf) ^ "not found")
                 in *)
              (* ( *)
              let _ = Stack.pop lookup_stack in
              Stack.push (arg, (Stack.copy context_stack)) lookup_stack;
              (* let new_context_stack = Stack.copy context_stack in
                 if mayBeTop context_stack cl then
                 ( *)
              lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),Binary_operator_equal_to, Var_formula(xn, (Stack.copy context_stack)))])
              (* ) *)
              (* else
                 failwith "context didn't match?" *)
            else
              let _ = print_endline "looking for right context" in
              (* gotta find the other node - if there isn't fails *)
              let right_node = matching_node graph node cur_context in
              Hashtbl.add graph node right_node;
              Stack.push cur_context context_stack;
              (* print_endline ("rightnode: " ^ (string_of_annotated_clause right_node)); *)
              (* let _ = Stack.pop lookup_stack in *)
              (* Stack.push (arg, "placeholder") lookup_stack; *)
              lookup lookup_stack node context_stack graph phi
        else
          let original_context_stack = Stack.copy context_stack in
          let cur_context = Stack.pop context_stack in
          if cur_context = cl then
            let _ = print_endline "TESTING TESTING TESTING" in
            let _ = print_endline "TESTING TESTING TESTING" in
            let _ = print_endline "TESTING TESTING TESTING" in
            let _ = print_endline "TESTING TESTING TESTING" in
            let _ = print_endline "TESTING TESTING TESTING" in
            let _ = print_endline "TESTING TESTING TESTING" in
            let _ = Stack.push (xf, (Stack.copy context_stack)) lookup_stack in
            lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),
                                                                            Binary_operator_equal_to, (Var_formula(cur_var, (Stack.copy context_stack))))])
          else
            let _ = print_endline "looking for right context" in
            (* gotta find the other node - if there isn't fails *)
            let right_node = matching_node graph node cur_context in
            Hashtbl.add graph node right_node;
            Stack.push cur_context context_stack;
            print_endline ("rightnode: " ^ (string_of_annotated_clause right_node));
            lookup lookup_stack node context_stack graph phi
      | Conditional_body(_,_,_,_) ->
        if cur_var <> param then
          let _ = print_endline "QWE QWE QWE QWE QWE QWE" in
          let _ = print_endline "QWE QWE QWE QWE QWE QWE" in
          let _ = print_endline "QWE QWE QWE QWE QWE QWE" in
          let _ = print_endline "QWE QWE QWE QWE QWE QWE" in
          let _ = print_endline "QWE QWE QWE QWE QWE QWE" in
          (* eventually need to be more careful with context stack here *)
          let original_context_stack = Stack.copy context_stack in
          let _ = Stack.pop context_stack in
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),
                                                                          Binary_operator_equal_to, (Var_formula(cur_var, (Stack.copy context_stack))))])
        else
        if Stack.is_empty context_stack then
          failwith "d"
        else
          let _ = Stack.pop lookup_stack in
          Stack.push (arg, (Stack.copy context_stack)) lookup_stack;
          let original_context_stack = Stack.copy context_stack in
          let _ = Stack.pop context_stack in
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),
                                                                          Binary_operator_equal_to, Var_formula(arg, (Stack.copy context_stack)))])
      | _ ->
        failwith "exit clause context is not appl or cond"
    end
  | Exit_clause(original_program_point, new_program_point, (Clause(_, body) as cl)) ->
    begin
      match body with
      | Appl_body(xf, _) ->
        if original_program_point = cur_var then
          let new_stack = Stack.create () in
          Stack.push (xf, (Stack.copy context_stack)) new_stack;
          let context_stack_copy = Stack.copy context_stack in
          let _ =
            try
              lookup new_stack (Unannotated_clause(cl)) context_stack_copy graph phi
            with
            | Not_found -> failwith ("function " ^ (string_of_var xf) ^ "not found")
          in
          let original_context_stack = Stack.copy context_stack in
          Stack.push cl context_stack;
          let _ = Stack.pop lookup_stack in
          Stack.push (new_program_point, (Stack.copy context_stack)) lookup_stack;
          print_endline ("\nFunction found, proceeding");
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy context_stack)))])
        else
          (* if not relevant, skip it *)
          let new_a1 = get_non_exit_clause_node graph node in
          lookup lookup_stack new_a1 context_stack graph phi
      | Conditional_body(x,p,Function_value(_,Expr(f1)),_) ->
        let original_context_stack = Stack.copy context_stack in
        Stack.push cl context_stack;

        let original_context_stack_2 = Stack.copy context_stack in

        print_endline "asda;sldkfjas;ldkfj;aslkdfj;aslkdfj;alskdfj;alskdfj;alskdfja;slkfj";
        print_context_stack original_context_stack_2;

        let original_context_stack_3 = Stack.copy context_stack in

        print_endline "asda;sldkfjas;ldkfj;aslkdfj;aslkdfj;alskdfj;alskdfj;alskdfja;slkfj";
        print_context_stack original_context_stack_3;

        if original_program_point <> cur_var then
          lookup lookup_stack a1 context_stack graph phi
        else
          let _ = Stack.pop lookup_stack in
          Stack.push (new_program_point, (Stack.copy context_stack)) lookup_stack;
          if (List.length (Hashtbl.find_all graph node)) > 2 then (* both branches *)
            (
              (* let new_queue = Queue.create () in *)
              let new_lookup_stack_1 = Stack.copy lookup_stack in
              let new_lookup_stack_2 = Stack.copy lookup_stack in
              let true_exit_node, false_exit_node = get_exit_nodes graph node (rv f1) in
              let false_branch_original_program_point, false_branch_new_program_point =
                match false_exit_node with
                | Exit_clause(false_branch_old_var, false_branch_new_var, _) ->
                  let _ = Stack.pop new_lookup_stack_2 in
                  Stack.push (false_branch_new_var, (Stack.copy context_stack)) new_lookup_stack_2;
                  false_branch_old_var, false_branch_new_var
                | _ ->
                  failwith "This should never happen"
              in
              (* let new_context_stack_1 = Stack.copy context_stack in
                 let new_context_stack_2 = Stack.copy context_stack in *)
              let new_graph_1 = Hashtbl.copy graph in
              let new_graph_2 = Hashtbl.copy graph in

              (* so find the argument by lookup. Then take the phi from that *)
              let temp_lookup_stack = Stack.create () in
              Stack.push (x, (Stack.copy context_stack)) temp_lookup_stack;
              let new_phi =
                begin
                  match lookup temp_lookup_stack a1 context_stack graph phi with
                  | Done(_, phi_arg) ->
                    phi_arg
                  | _ ->
                    failwith "had to make decision to find arg - can probably solve this by doing eval - but maybe not"
                end
              in
              print_endline ":LSKFJS:LDKFJSLDKFJSLKDFJLSDFJ";
              print_context_stack original_context_stack_2;
              print_context_stack original_context_stack_3;
              print_context_stack original_context_stack_3;
              print_endline (string_of_formula_2 (Binary_formula(Var_formula(original_program_point, (Stack.copy original_context_stack)),
                                                                 Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy original_context_stack_2)))));

              (* let new_phi = new_phi@[Binary_formula(Var_formula(original_program_point, original_context_stack), *)
              (* Binary_operator_equal_to, Var_formula(new_program_point, original_context_stack_2))] in *)

              print_phi new_phi;
              let new_phi_1 = new_phi@[Binary_formula(Var_formula(x, (Stack.copy context_stack)), Binary_operator_equal_to, Pattern_formula(p))]
                              @[Binary_formula(Var_formula(original_program_point, (Stack.copy original_context_stack)),
                                               Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy original_context_stack_2)))]
              in
              let new_phi_2 = new_phi@[Negated_formula(Binary_formula(Var_formula(x, (Stack.copy context_stack)), Binary_operator_equal_to, Pattern_formula(p)))]
                              @[Binary_formula(Var_formula(false_branch_original_program_point, (Stack.copy original_context_stack)),
                                               Binary_operator_equal_to, Var_formula(false_branch_new_program_point, (Stack.copy original_context_stack_3)))]
              in

              print_phi new_phi_1;
              print_phi new_phi_2;

              Double_working(Working(new_lookup_stack_1, true_exit_node, original_context_stack_2, new_graph_1, new_phi_1),
                             Working(new_lookup_stack_2, false_exit_node, original_context_stack_3, new_graph_2, new_phi_2))
            )
          else
            (* think we don't have to add anything else to the phi b/c dppa found only one option *)
            lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(original_program_point, (Stack.copy context_stack)),
                                                                            Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy context_stack)))])
      | _ ->
        failwith "exit clause context is not appl or cond"
    end
  (* arg, pattern, then, else *)
  | Conditional_enter_clause(_)
  | Conditional_exit_clause(_,_,_,_,_,_) ->
    failwith "will I ever get this?"
  | Start_clause(_)
  | End_clause(_) ->
    (* think we just skip *)
    lookup lookup_stack a1 context_stack graph phi
  | Junk_clause ->
    failwith "todo"

and eval_helper queue prompt_user : Core_ast.value * formula list =
  if Queue.length queue > 0 then
    let state = Queue.take queue in
    match state with
    | Done(v, phi) ->
      print_endline ("Phi: " ^ (string_of_phi phi));
      (v, phi)
    | Working(l, n, c, g, p) ->
      print_endline "Queue interaction";
      let new_state = lookup l n c g p in
      begin
        match new_state with
        | Done(v, phi) ->
          let phi = trim_list phi [] in
          let _ = Sys.command ("python /home/theodore/research/odefa/src/core-interpreter/test.py \"" ^ (string_of_phi phi) ^ "\"") in
          if prompt_user then
            (
              print_endline "Do you want to continue? 1 to continue searching, 2 to take values found";
              let i = read_int () in
              if i = 1 then
                (
                  print_endline "continuing..." (* do nothing *);
                  eval_helper queue prompt_user
                )
              else
                v, phi
            )
          else
            v,phi (* hmm so if we don't prompt, can only take first guess?... *)
        | Working(_) ->
          (
            print_endline "new state was not terminal";
            eval_helper queue true
          )
        | Double_working(state1, state2) ->
          (
            print_endline "We got two states";
            Queue.add state1 queue;
            Queue.add state2 queue;
            eval_helper queue true
          )
      end
    | Double_working(state1, state2) ->
      (
        Queue.add state1 queue;
        Queue.add state2 queue;
        eval_helper queue true
      )
  else
    (
      failwith "queue empty - guess we are done";
    )
;;

let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t * formula * input_mapping =
  (* make dictionary *)
  let map = make_map cls (Hashtbl.create 10) in
  (* print_intermediate_map map; *)

  (* Parse through ddpa graph and construct graph, using map to get clause bodies *)
  let json = (List.hd (Yojson.Basic.from_file "ddpa_graphs.json" |> to_list)) |> member "graph" |> member "ddpa_graph" |> to_list in
  let json_of_graph = List.hd (List.tl json) in
  let elements = json_of_graph |> member "elements" |> to_list in
  let graph = initialize_graph map elements (Hashtbl.create 10) in

  print_graph graph;

  (* make lookup and context stacks *)
  let lookup_stack:(var * (clause Stack.t)) Stack.t = Stack.create () in
  let context_stack:(clause) Stack.t = Stack.create () in
  let phi:formula list = [] in

  (* remove last clause from program and store program point specified by it *)
  let clause_list, starting_program_point =
    match (List.rev cls) with
    | [] -> failwith "empty program"
    | Clause(_, x) :: tail ->
      begin
        match x with
        | Var_body(v) ->
          (List.rev tail), v
        | _ ->
          failwith "last line was not an alias"
      end
  in

  let _ =
    try
      let _ = find_clause_by_var graph starting_program_point in
      "placeholder"
    with
    | Not_found -> failwith "unreachable" (* assumes if node isn't in graph its unreachable *)
  in

  Stack.push (starting_program_point, (Stack.copy context_stack)) lookup_stack;
  let queue = Queue.create () in
  Queue.add (Working(lookup_stack, (find_starting_node_2 graph starting_program_point), context_stack, graph, phi)) queue;
  let output_val,phi = eval_helper queue true in

  print_endline ("Output val: " ^ (string_of_value output_val));
  print_phi phi;

  let _ = Sys.command ("python /home/theodore/research/odefa/src/core-interpreter/test.py \"" ^ (string_of_phi phi) ^ "\"") in

  (* let _ = handle_unix_error script (string_of_phi phi) in *)

  let rx = rv (List.rev clause_list) in

  let env = Core_interpreter.Environment.create 10 in
  Core_interpreter.Environment.add env rx output_val;

  rx, env, List.hd phi, Iota.create 10



;;
