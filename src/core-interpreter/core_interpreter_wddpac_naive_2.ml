(* open Batteries;;
open Jhupllib;;

open Core_ast;; *)
(* open Core_ast_pp;; *)
(* open Pp_utils;; *)
(* open Unbounded_context_stack_naive;; *)
(* open Lookup_stack;; *)
(* open Wddpac_graph;; *)

(* open Stack;; *)

(* returns the last program point of the program *)
(* let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;; *)



let eval   =
  (* let context_stack = Stack.create () in
  let lookup_stack = Stack.create () in *)

  (* let graph = initialize_graph cls in *)

  (* let rx = rv cls in *)
  ()



  (* let initial_graph, initial_node = initialize_graph cls in
  let complete_graph, valid = create_graph initial_graph initial_node context_stack in

  if not valid then raise @@ Utils.Invariant_failure "invalid graph at the end" else
    let v = lookup complete_graph (rv cls) (End_clause(rv cls)) lookup_stack context_stack in
    match v with
    | None -> raise @@ Utils.Invariant_failure "Unable to evaluate"
    | Some(v) -> v *)
;;
