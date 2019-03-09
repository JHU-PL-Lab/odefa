open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_interpreter_utils;;

open Unix;;
open Yojson.Basic.Util;;

type program_state = Done of (Core_ast.value * formula list) |
                     Working of ((Core_ast.var * string) Stack.t * annotated_clause * Core_ast.clause Stack.t * (annotated_clause,annotated_clause) Hashtbl.t * formula list);;

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
let rec lookup lookup_stack (node:annotated_clause) context_stack graph phi: program_state =
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
            Done(v, phi@[Binary_formula(Var_formula(x), Binary_operator_equal_to, Value_formula(v))])
        | Var_body(var) ->
          let _ = Stack.pop lookup_stack in
          Stack.push (var, "placeholder") lookup_stack;
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Var_formula(var))])
        | Input ->
          (* solve(Or(x <= 1, 1 <= x)) *)
          let negCover = Binary_formula(Var_formula(x),Binary_operator_int_less_than_or_equal_to, Value_formula(Value_int(1))) in
          let posCover = Binary_formula(Value_formula(Value_int(1)), Binary_operator_int_less_than_or_equal_to, Var_formula(x)) in
          let existence_formula = Binary_formula(negCover, Binary_operator_bool_or, posCover) in
          Done(Value_int(0),phi@[existence_formula]) (* pretty sure the value here is garbage - well what if we depend on it later? *)
        | Appl_body(_,_) ->
          failwith "appl body encountered - this should never happen I think"
        | Unary_operation_body(op, v1) ->
          let _ = Stack.pop lookup_stack in
          Stack.push (v1, "placeholder") lookup_stack;
          let q1 = Queue.create () in
          Queue.add (Working(lookup_stack, a1, context_stack, graph, phi)) q1;
          let v2, phi2 = eval_helper q1 false in
          begin
            match op with
            | Unary_operator_bool_not ->
              begin
                match v2 with
                | Value_bool(b) -> Done(Value_bool(not b), phi@phi2@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Negated_formula(Var_formula(v1)))])
                | _ -> raise @@ Utils.Invariant_failure "non-bool expr with not operator"
              end
            | Unary_operator_bool_coin_flip ->
              failwith "unimplemented coin flip"
          end
        | Binary_operation_body(v1, op, v2) ->
          let _ = Stack.pop lookup_stack in
          let lookup_stack1 = Stack.copy lookup_stack in
          Stack.push (v1, "placeholder") lookup_stack1;
          let context_stack1 = Stack.copy context_stack in
          let context_stack2 = Stack.copy context_stack in
          let q1 = Queue.create () in
          Queue.add (Working(lookup_stack1, a1, context_stack1, graph, phi)) q1;
          let left_value, phi1 = eval_helper q1 false in
          let lookup_stack2 = Stack.copy lookup_stack in
          Stack.push (v2, "placeholder") lookup_stack2;
          let q2 = Queue.create () in
          Queue.add (Working(lookup_stack2, a1, context_stack2, graph, phi)) q2;
          let right_value, phi2 = eval_helper q2 false in
          let phi = phi@phi1@phi2 in
          begin
            match op with
            | Binary_operator_plus ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Done(Value_int(i1 + i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_plus, Var_formula(v2)))])
                | _ -> failwith "tried to add non-int"
              end
            | Binary_operator_int_minus ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Done(Value_int(i1 - i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_int_minus, Var_formula(v2)))])
                | _ -> failwith "tried to subtract non-int"
              end
            | Binary_operator_int_less_than ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Done(Value_bool(i1 < i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_int_less_than, Var_formula(v2)))])
                | _ -> failwith "tried to less than non-int"
              end
            | Binary_operator_int_less_than_or_equal_to ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Done(Value_bool(i1 <= i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_int_less_than_or_equal_to, Var_formula(v2)))])
                | _ -> failwith "tried to less or equal than non-int"
              end
            | Binary_operator_equal_to ->
              let new_formula = phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_equal_to, Var_formula(v2)))] in
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("equal evaluated to " ^ string_of_bool(i1=i2)); *)
                  Done(Value_bool(i1 = i2), new_formula)
                | Value_bool(b1), Value_bool(b2) -> Done(Value_bool(b1 = b2), new_formula)
                | _ -> failwith "tried to equal two different types"
              end
            | Binary_operator_bool_and ->
              begin
                match left_value, right_value with
                | Value_bool(b1), Value_bool(b2) -> Done(Value_bool(b1 && b2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_bool_and, Var_formula(v2)))])
                | _ -> failwith "tried to and non-booleans"
              end
            | Binary_operator_bool_or ->
              begin
                match left_value, right_value with
                | Value_bool(b1), Value_bool(b2) -> Done(Value_bool(b1 || b2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_bool_or, Var_formula(v2)))])
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
            let cur_context = Stack.pop context_stack in
            if cur_context = cl then
              let new_stack = Stack.create () in
              Stack.push (xf, "placeholder") new_stack;
              let new_context_stack = Stack.copy context_stack in
              let _ =
                try
                  lookup new_stack node new_context_stack graph phi
                with
                | Not_found -> failwith ("function " ^ (string_of_var xf) ^ "not found")
              in
              let _ = Stack.pop lookup_stack in
              Stack.push (arg, "placeholder") lookup_stack;
              let new_context_stack = Stack.copy context_stack in
              if mayBeTop context_stack cl then
                (
                  lookup lookup_stack a1 new_context_stack graph (phi@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Var_formula(xn))])
                )
              else
                failwith "context didn't match?"
            else
              (* gotta find the other node - if there isn't fails *)
              let right_node = matching_node graph node cur_context in
              Hashtbl.add graph node right_node;
              Stack.push cur_context context_stack;
              (* print_endline ("rightnode: " ^ (string_of_annotated_clause right_node)); *)
              (* let _ = Stack.pop lookup_stack in *)
              (* Stack.push (arg, "placeholder") lookup_stack; *)
              lookup lookup_stack node context_stack graph phi
        else
          let _ = Stack.push (xf, "placeholder") lookup_stack in
          let _ =
            if not (Stack.is_empty context_stack) then
              let _ = Stack.pop context_stack in
              "junk"
            else
              "junk"
          in
          (* lookup lookup_stack a1 (context_stack) graph (phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Var_formula(xn))]) *)
          lookup lookup_stack a1 (context_stack) graph phi
      | Conditional_body(_,_,_,_) ->
        if cur_var <> param then
          (* eventually need to be more careful with context stack here *)
          let _ = Stack.pop context_stack in
          lookup lookup_stack a1 context_stack graph phi
        else
        if Stack.is_empty context_stack then
          failwith "d"
        else
          let _ = Stack.pop lookup_stack in
          Stack.push (arg, "placeholder") lookup_stack;
          let _ = Stack.pop context_stack in
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Var_formula(arg))])
      | _ ->
        failwith "exit clause context is not appl or cond"
    end
  | Exit_clause(original_program_point, new_program_point, (Clause(_, body) as cl)) ->
    begin
      match body with
      | Appl_body(xf, _) ->
        if original_program_point = cur_var then
          let new_stack = Stack.create () in
          Stack.push (xf, "placeholder") new_stack;
          let _ =
            try
              lookup new_stack (Unannotated_clause(cl)) context_stack graph phi
            with
            | Not_found -> failwith ("function " ^ (string_of_var xf) ^ "not found")
          in
          (* let rv_xf = rv_of_function f in *)
          let _ = Stack.pop lookup_stack in
          Stack.push (new_program_point, "placeholder") lookup_stack;
          if mayBeTop context_stack cl then
            (
              Stack.push cl context_stack;
              print_endline ("\nFunction found, proceeding");
              lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Var_formula(new_program_point))])
            )
          else
            failwith "context didn't match?"
        else
          failwith "exit clause: vars didn't match"
      | Conditional_body(x,p,Function_value(_,Expr(f1)),_) ->
        (* we want to create a second eval helper call with the queue having two options - true and false branches *)
        (* but this only happens when there are multiple options to choose from *)
        Stack.push cl context_stack;
        if original_program_point <> cur_var then
          (* ignoring contexts here *)
          lookup lookup_stack a1 context_stack graph phi
        else
          let _ = Stack.pop lookup_stack in
          Stack.push (new_program_point, "placeholder") lookup_stack;
          if (List.length (Hashtbl.find_all graph node)) > 2 then (* both branches *)
            (
              let new_queue = Queue.create () in
              let new_lookup_stack_1 = Stack.copy lookup_stack in
              let new_lookup_stack_2 = Stack.copy lookup_stack in
              let true_exit_node, false_exit_node = get_exit_nodes graph node (rv f1) in
              let _ =
                match false_exit_node with
                | Exit_clause(_, false_branch_new_var, _) ->
                  let _ = Stack.pop new_lookup_stack_2 in
                  Stack.push (false_branch_new_var, "placeholder") new_lookup_stack_2;
                | _ ->
                  failwith "This should never happen"
              in
              let new_context_stack_1 = Stack.copy context_stack in
              let new_context_stack_2 = Stack.copy context_stack in
              let new_graph_1 = Hashtbl.copy graph in
              let new_graph_2 = Hashtbl.copy graph in
              let new_phi_1 = create_list_copy phi in
              let new_phi_2 = create_list_copy phi in

              Queue.add (Working(new_lookup_stack_1, true_exit_node, new_context_stack_1, new_graph_1, (new_phi_1@[Binary_formula(Var_formula(x), Binary_operator_equal_to, Pattern_formula(p))]))) new_queue;
              Queue.add (Working(new_lookup_stack_2, false_exit_node, new_context_stack_2, new_graph_2, (new_phi_2@[Negated_formula(Binary_formula(Var_formula(x), Binary_operator_equal_to, Pattern_formula(p)))]))) new_queue;

              let v, formulas = eval_helper new_queue false in
              Done(v, formulas)
            )
          else
            (* think we don't have to add anything else to the phi b/c dppa found only one option *)
            lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(original_program_point), Binary_operator_equal_to, Var_formula(new_program_point))])
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
      let _ =
        begin
          match new_state with
          | Done(_, phi) ->
            let _ = Sys.command ("python /home/theodore/research/odefa/src/core-interpreter/test.py \"" ^ (string_of_phi phi) ^ "\"") in
            if prompt_user then
              (
                print_endline "Do you want to continue? 1 to continue, 2 to exit";
                let i = read_int () in
                if i = 1 then
                  print_endline "continuing..." (* do nothing *)
                else
                  exit 0
              )
          | Working(_) ->
            print_endline "new state was not terminal"
        end
      in
      Queue.add new_state queue;
      eval_helper queue true
  else
    failwith "queue empty - guess we are done"
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
  (* let lookup_stack:(var * formula) Stack.t = Stack.create () in *)
  let lookup_stack:(var * string) Stack.t = Stack.create () in
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

  Stack.push (starting_program_point, "placeholder") lookup_stack;
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
