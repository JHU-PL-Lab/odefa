open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_interpreter_utils;;

(* open Sys;; *)
open Unix;;
open Yojson.Basic.Util;;

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

let script formulas_list =
  execv "/usr/bin/python" [| "python";"/home/theodore/research/odefa/src/core-interpreter/test.py";formulas_list|]
;;

let rec make_map clauses graph : (var,clause_body) Hashtbl.t =
  match clauses with
  | [] -> graph
  | Clause(var, body) :: tail ->
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
    let node_to_add = Hashtbl.find map var in
    Unannotated_clause(Clause(var, node_to_add))
  else if node_type = "Start_clause" then
    Start_clause
  else if node_type = "End_clause" then
    End_clause
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
        | _ ->
          failwith "not implemented yet"
      end
  | Enter_clause(_) ->
    failwith "todo"
  | Exit_clause(_) ->
    failwith "todo"
  (* arg, pattern, then, else *)
  | Conditional_enter_clause(_) ->
    failwith "todo"
  (* Conditional_enter_clause, x from x = conditional *)
  | Conditional_exit_clause(_,_,_,_,_,_) ->
    failwith "todo"
  | Start_clause ->
    failwith "todo"
  | End_clause ->
    failwith "todo"
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

  Stack.push (starting_program_point, "placeholder") lookup_stack;
  let output_val,phi = lookup lookup_stack (find_starting_node_2 graph starting_program_point) context_stack graph phi in

  print_endline ("Output var: " ^ (string_of_value output_val));
  print_phi phi;

  let _ = handle_unix_error script (string_of_phi phi) in


  failwith "This is just here to make the compiler happy"


;;
