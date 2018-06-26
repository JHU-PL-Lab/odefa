open Batteries;;
open Jhupllib;;

open Core_ast;;
open Wddpac_graph;;
(* open Core_ast_pp;; *)
(* open Pp_utils;; *)
(* open Unbounded_context_stack_naive;; *)
(* open Lookup_stack;; *)

(* open Stack;; *)

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

let rec initialize_graph (cls: clause list) (graph: wddpac_graph) : wddpac_graph =
  match cls with
  | [] ->
    graph
  | (Clause(x, body) as cl) :: tail ->
    begin
      match body with
      | Value_body(v) -> ()
      | Var_body(v) -> ()
      | Conditional_body(var, pattern, fcn_value_1, fcn_value_2) -> ()
      | _ ->
        add_edge Wddpac_edge() graph;
    end
;;



let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t =
  (* let context_stack = Stack.create () in
  let lookup_stack = Stack.create () in *)

  (* let graph = initialize_graph (List.rev cls) in *)

  let rx = rv cls in

  (* this is to fit the return value the toploop expects *)
  let env = Core_interpreter.Environment.create 10 in
  let v = Value_bool(true) in (* to be replaced by lookup *)
  Core_interpreter.Environment.add env rx v;
  rx, env



  (* let initial_graph, initial_node = initialize_graph cls in
  let complete_graph, valid = create_graph initial_graph initial_node context_stack in

  if not valid then raise @@ Utils.Invariant_failure "invalid graph at the end" else
    let v = lookup complete_graph (rv cls) (End_clause(rv cls)) lookup_stack context_stack in
    match v with
    | None -> raise @@ Utils.Invariant_failure "Unable to evaluate"
    | Some(v) -> v *)
;;
