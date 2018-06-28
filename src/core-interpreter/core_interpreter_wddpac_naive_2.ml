open Batteries;;
open Jhupllib;;

open Core_ast;;
(* open Hashtbl;; *)
(* open Wddpac_graph;; *)
(* open Core_ast_pp;; *)
(* open Pp_utils;; *)
(* open Unbounded_context_stack_naive;; *)
(* open Lookup_stack;; *)

(* open Stack;; *)


module Annotated_Clause =
struct
  type t = var
  let equal = equal_var
  let hash = Hashtbl.hash
end;;

type annotated_clause =
  | Unannotated_clause of clause
  | Enter_clause of var * var * clause
  | Exit_clause of var * var * clause
  | Start_clause
  (** This variable is the return variable of the block that this clause
      starts. Removed the of var for now *)
  | End_clause of var
  (** This variable is the return variable of the block that this clause
      ends. *)
[@@deriving ord, eq, to_yojson]
;;

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

(*
  application is not dealt with completely here.
  the rule to be implemented asks us to wire when we encounter the application durig lookup
*)
let rec initialize_graph (prev: annotated_clause) (cls: clause list) (graph: (annotated_clause, annotated_clause) Hashtbl.t) :
  (annotated_clause, annotated_clause) Hashtbl.t =
  match cls with
  | [] ->
    Hashtbl.add graph prev Start_clause;
    graph
  | (Clause(x, body) as cl) :: tail ->
    begin
      match body with
      | Conditional_body(_,_,_,_) -> failwith "Not sure how to wire in"
      (* | Conditional_body(var, pattern, fcn_value_1, fcn_value_2) -> failwith "df" *)
      | _ ->
        Hashtbl.add graph (Unannotated_clause(cl)) prev;
        initialize_graph (Unannotated_clause(cl)) tail graph
    end
;;

(*
  lookup function
*)
let rec lookup lookup_stack (node:annotated_clause) context_stack graph: Core_ast.value =
  let cur_var = Stack.top lookup_stack in
  match node with
  | Unannotated_clause(cl) ->
    begin
      let Clause(x, body) = cl in
      if x != cur_var then
        (* skip. rule 10 *)
        lookup lookup_stack (Hashtbl.find graph node) context_stack
      else
        begin
          match body with
          | Value_body(v) ->
            (* check size of lookup stack either rule 1 or 3 *)
            (* value discovery. rule 1 *)
            (* TODO: check formula *)
            v
          | Var_body(v) ->
            (* alias. rule 4 *)

        end
    end
  | Enter_clause(v1,v2,cl) ->
    failwith "d"
  | Exit_clause(v1,v2,cl) ->
    failwith "d"
  | Start_clause ->
    failwith "d"
  | End_clause(v) ->
    failwith "d"
;;

(* and clause_body =
    | Value_body of value
  | Var_body of var
  | Appl_body of var * var
  | Conditional_body of var * pattern * function_value * function_value
  | Projection_body of var * ident
  | Deref_body of var
  | Update_body of var * var
  | Binary_operation_body of var * binary_operator * var
  | Unary_operation_body of unary_operator * var
  | Input
[@@deriving eq, ord, to_yojson]

(** A type to represent clauses. *)
and clause =
    | Clause of var * clause_body
[@@deriving eq, ord, to_yojson] *)



let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t =
  let context_stack = Stack.create () in
  let lookup_stack = Stack.create () in

  let rx = rv cls in

  let graph:(annotated_clause, annotated_clause) Hashtbl.t = initialize_graph (End_clause(rx)) (List.rev cls) (Hashtbl.create 10) in


  (* this is to fit the return value the toploop expects *)
  let env = Core_interpreter.Environment.create 10 in

  let v = lookup lookup_stack ___ context_stack graph in (* to be replaced by lookup *)
  (* likely an Unannotated_clause like x=x *)
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
