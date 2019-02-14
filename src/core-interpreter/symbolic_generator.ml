open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_interpreter_utils;;

open Unix;;
open Yojson.Basic.Util;;

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
let rec lookup lookup_stack (node:annotated_clause) context_stack graph phi: (Core_ast.value * formula list) =
  let (cur_var, _) = Stack.top lookup_stack in
  let a1 =
    try
      Hashtbl.find graph node
    with
    | Not_found -> failwith "not in graph"
  in
  (* print_endline ("\nCurrent lookup variable: " ^ string_of_var cur_var); *)
  (* print_endline ("Current node: " ^ string_of_annotated_clause node); *)
  (* print_endline ("a1 node: " ^ string_of_annotated_clause a1); *)
  (* print_stack lookup_stack; *)
  (* print_context_stack context_stack; *)
  match a1 with
  | Unannotated_clause(cl) ->
    let Clause(x, body) = cl in
    if x <> cur_var then
      (
        (* print_endline "skip"; *)
        lookup lookup_stack a1 context_stack graph phi
      )
    else
      begin
        match body with
        | Value_body(v) ->
          if Stack.length lookup_stack > 1 then
            let _ = Stack.pop lookup_stack in
            lookup lookup_stack a1 context_stack graph phi
          else
            v, phi@[Binary_formula(Var_formula(x), Binary_operator_equal_to, Value_formula(v))]
        | Var_body(var) ->
          let _ = Stack.pop lookup_stack in
          Stack.push (var, "placeholder") lookup_stack;
          lookup lookup_stack a1 context_stack graph (phi@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Var_formula(var))])
        | Input ->
          (* solve(Or(x <= 1, 1 <= x)) *)
          let negCover = Binary_formula(Var_formula(x),Binary_operator_int_less_than_or_equal_to, Value_formula(Value_int(1))) in
          let posCover = Binary_formula(Value_formula(Value_int(1)), Binary_operator_int_less_than_or_equal_to, Var_formula(x)) in
          let existence_formula = Binary_formula(negCover, Binary_operator_bool_or, posCover) in
          Value_int(0),phi@[existence_formula] (* pretty sure the value here is garbage - well what if we depend on it later? *)
        | Appl_body(_,_) ->
          failwith "appl body encountered - this should never happen I think"
        | Unary_operation_body(op, v1) ->
          let _ = Stack.pop lookup_stack in
          Stack.push (v1, "placeholder") lookup_stack;
          let v2, phi2 = lookup lookup_stack a1 context_stack graph phi in
          begin
            match op with
            | Unary_operator_bool_not ->
              begin
                match v2 with
                | Value_bool(b) -> Value_bool(not b), phi@phi2@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Negated_formula(Var_formula(v1)))]
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
          let left_value, phi1 = lookup lookup_stack1 a1 context_stack graph phi in
          let lookup_stack2 = Stack.copy lookup_stack in
          Stack.push (v2, "placeholder") lookup_stack2;
          let right_value, phi2 = lookup lookup_stack2 a1 context_stack1 graph phi in
          let phi = phi@phi1@phi2 in
          begin
            match op with
            | Binary_operator_plus ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Value_int(i1 + i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_plus, Var_formula(v2)))]
                | _ -> failwith "tried to add non-int"
              end
            | Binary_operator_int_minus ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Value_int(i1 - i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_int_minus, Var_formula(v2)))]
                | _ -> failwith "tried to subtract non-int"
              end
            | Binary_operator_int_less_than ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Value_bool(i1 < i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_int_less_than, Var_formula(v2)))]
                | _ -> failwith "tried to less than non-int"
              end
            | Binary_operator_int_less_than_or_equal_to ->
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("left side evaulated to: " ^ (string_of_value left_value)); *)
                  (* print_endline ("plus evaluated to " ^ string_of_int(i1+i2)); *)
                  Value_bool(i1 <= i2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_int_less_than_or_equal_to, Var_formula(v2)))]
                | _ -> failwith "tried to less or equal than non-int"
              end
            | Binary_operator_equal_to ->
              let new_formula = phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_equal_to, Var_formula(v2)))] in
              begin
                match left_value, right_value with
                | Value_int(i1), Value_int(i2) ->
                  (* print_endline ("equal evaluated to " ^ string_of_bool(i1=i2)); *)
                  Value_bool(i1 = i2), new_formula
                | Value_bool(b1), Value_bool(b2) -> Value_bool(b1 = b2), new_formula
                | _ -> failwith "tried to equal two different types"
              end
            | Binary_operator_bool_and ->
              begin
                match left_value, right_value with
                | Value_bool(b1), Value_bool(b2) -> Value_bool(b1 && b2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_bool_and, Var_formula(v2)))]
                | _ -> failwith "tried to and non-booleans"
              end
            | Binary_operator_bool_or ->
              begin
                match left_value, right_value with
                | Value_bool(b1), Value_bool(b2) -> Value_bool(b1 || b2), phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Binary_formula(Var_formula(v1), Binary_operator_bool_or, Var_formula(v2)))]
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
          let new_stack = Stack.create () in
          Stack.push (xf, "placeholder") new_stack;
          let new_context_stack = context_stack in
          let _ = Stack.pop context_stack in
          let _ =
            try
              lookup new_stack a1 new_context_stack graph phi
            with
            | Not_found -> failwith ("function " ^ (string_of_var xf) ^ "not found")
          in
          let _ = Stack.pop lookup_stack in
          Stack.push (arg, "placeholder") lookup_stack;
          if mayBeTop context_stack cl then
            (
              lookup lookup_stack a1 new_context_stack graph (phi@[Binary_formula(Var_formula(cur_var),Binary_operator_equal_to, Var_formula(xn))])
            )
          else
            failwith "context didn't match?"
        else
          let _ = Stack.push (xf, "placeholder") lookup_stack in
          let _ = Stack.pop context_stack in
          (* lookup lookup_stack a1 (context_stack) graph (phi@[Binary_formula(Var_formula(cur_var), Binary_operator_equal_to, Var_formula(xn))]) *)
          lookup lookup_stack a1 (context_stack) graph phi
      | Conditional_body(_,_,_,_) ->
        failwith "conditional todo"
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
      | Conditional_body(x,p,Function_value(_,Expr(f1)),Function_value(_,Expr(f2))) ->
        let new_lookup_stack = Stack.copy lookup_stack in
        let _ = Stack.pop lookup_stack in
        Stack.push (x,"placehodler") new_lookup_stack;
        let new_context_stack = Stack.copy context_stack in
        let xv,_ = lookup new_lookup_stack a1 new_context_stack graph phi in
        (* evaulate *)
        let rv_of_function =
          if matches xv p then
            rv f1
          else
            rv f2
        in
        Stack.push (rv_of_function,"placehoder") lookup_stack;
        Stack.push cl context_stack;
        failwith "d"
      | _ ->
        failwith "exit clause context is not appl or cond"
    end

  (* arg, pattern, then, else *)
  | Conditional_enter_clause(_) ->
    failwith "todo"
  (* Conditional_enter_clause, x from x = conditional *)
  | Conditional_exit_clause(_,_,_,_,_,_) ->
    failwith "todo"
  | Start_clause(_)
  | End_clause(_) ->
    (* think we just skip *)
    lookup lookup_stack a1 context_stack graph phi
  | Junk_clause ->
    failwith "todo"
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
  let _, starting_program_point =
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
  let output_val,phi = lookup lookup_stack (find_starting_node_2 graph starting_program_point) context_stack graph phi in

  print_endline ("Output var: " ^ (string_of_value output_val));
  print_phi phi;

  let _ = handle_unix_error script (string_of_phi phi) in


  failwith "This is just here to make the compiler happy"


;;
