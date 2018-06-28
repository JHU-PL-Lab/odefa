open Batteries;;
open Jhupllib;;

open Core_ast;;
open Formula;;
(* open Core_ast_pp;; *)
(* open Pp_utils;; *)

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
  | End_clause
  (** This variable is the return variable of the block that this clause
      ends. Removed the of var for now *)
[@@deriving ord, eq, to_yojson]
;;

let string_of_var v : string =
  let Var(i, _) = v in
  let Ident(s) = i in
  s
;;

let string_of_value v : string =
  match v with
  | Value_int(i) ->
    string_of_int i
  | Value_bool(b) ->
    if b then "true" else "false"
  | _ ->
    "some other value"
;;


let string_of_annotated_clause cl : string =
  match cl with
  | Unannotated_clause(Clause(x,body)) ->
    string_of_var x ^ " = " ^
    begin
      match body with
      | Value_body(v) -> string_of_value v
      | Var_body(v) -> "variable " ^ string_of_var v
      | Input -> "input"
      | Appl_body(v1,v2) -> "fcn " ^ string_of_var v1 ^ " with arg " ^ string_of_var v2
      | _ -> "some other body"
    end
  | End_clause ->
    "End clause"
  | _ ->
    failwith "d"
;;

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

(*
  Adjacency list representation
  Annotated_Clause -> the nodes/clauses that are directly behind it.
  if a << b then b maps to a since we are traversing backwards
*)
let rec initialize_graph (prev: annotated_clause) (cls: clause list) (graph: (annotated_clause, annotated_clause) Hashtbl.t) :
  (annotated_clause, annotated_clause) Hashtbl.t =
  match cls with
  | [] ->
    Hashtbl.add graph prev Start_clause;
    graph
  | head :: tail ->
    Hashtbl.add graph prev (Unannotated_clause(head));
    initialize_graph (Unannotated_clause(head)) tail graph
;;

(*
  lookup function
*)
let rec lookup lookup_stack (node:annotated_clause) context_stack graph iota: Core_ast.value =
  let (cur_var, cur_formula) = Stack.top lookup_stack in
  print_endline ("\nCurrent lookup variable: " ^ string_of_var cur_var);
  print_endline ("Current node: " ^ string_of_annotated_clause node);
  match node with
  | Unannotated_clause(cl) ->
    begin
      let Clause(x, body) = cl in
      if x <> cur_var then
        (* rule 10: Skip *)
        (
          print_endline "skip";
          (* should probably add a try with here eventually *)
          lookup lookup_stack (Hashtbl.find graph node) context_stack graph iota
        )
      else
        begin
          match body with
          | Value_body(v) ->
            (* check size of lookup stack to determine if we use rule 1 or 3 *)
            if Stack.length lookup_stack = 1 then
              (* rule 1: value discovery *)
              (
                print_endline "value discovery";
                if check_formula (substitute_value cur_formula cur_var v) then v else failwith "I don't know what to do here. Its a dead end"
              )
            else
              (* rule 3: value discard *)
              (
                print_endline "value discard";
                let _ = Stack.pop lookup_stack in
                lookup lookup_stack node context_stack graph iota
              )
          | Var_body(v) ->
            (* rule 4: alias *)
            print_endline "alias";
            let _ = Stack.pop lookup_stack in
            Stack.push (v, (substitute_var cur_formula cur_var v)) lookup_stack;
            lookup lookup_stack (Hashtbl.find graph node) context_stack graph iota
          | Input ->
            (* rule 2: input. TODO: right now it guesses 5 for all input clauses *)
            begin
              try
                let v = Hashtbl.find iota cur_var in
                if check_formula (substitute_value cur_formula cur_var v) then v else failwith "I don't know what to do here. Its a dead end"
              with
              | Not_found ->
                let v = Value_int(5) in
                Hashtbl.add iota x v;
                if check_formula (substitute_value cur_formula x v) then v else failwith "I don't know what to do here" (* might make another return type *)
              | _ -> failwith "unhandled exception when looking up iota mapping"
            end
          | _ ->
            failwith "unannotated"
        end
    end
  | Enter_clause(_,_,_) ->
    failwith "enter"
  | Exit_clause(_,_,_) ->
    failwith "exit"
  | Start_clause ->
    failwith "start"
  | End_clause ->
    print_endline "end";
    let new_node = Hashtbl.find graph node in
    lookup lookup_stack new_node context_stack graph iota
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

(* record rules on page 37 *)


let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t =
  let context_stack = Stack.create () in
  let lookup_stack:(var * formula) Stack.t = Stack.create () in
  let iota:(Core_ast.var, Core_ast.value) Hashtbl.t = Hashtbl.create 10 in

  let rx = rv cls in
  (* start lookup with last program point *)
  Stack.push (rx, true_formula) lookup_stack;

  (* make graph *)
  let graph:(annotated_clause, annotated_clause) Hashtbl.t = initialize_graph (End_clause) (List.rev cls) (Hashtbl.create 10) in

  (* add in end_clause *)
  (* for now assume Unannotated_clause, not sure if it always will be *)
  Hashtbl.add graph End_clause (Unannotated_clause(List.hd (List.rev cls)));

  (* this is to fit the return value the toploop expects *)
  let env = Core_interpreter.Environment.create 10 in

  (* do lookup *)
  let v = lookup lookup_stack End_clause context_stack graph iota in
  Core_interpreter.Environment.add env rx v;
  rx, env



(*
   if not valid then raise @@ Utils.Invariant_failure "invalid graph at the end" else
   let v = lookup complete_graph (rv cls) (End_clause(rv cls)) lookup_stack context_stack in
   match v with
   | None -> raise @@ Utils.Invariant_failure "Unable to evaluate"
   | Some(v) -> v
 *)
;;
