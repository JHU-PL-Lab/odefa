(* file has string conversion methods and the annotated_clause type needed for the cfg construction *)
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Pp_utils;;

module Environment = Var_hashtbl;;

type evaluation_environment = value Environment.t;;
let pp_evaluation_environment = pp_map pp_var pp_value Environment.enum;;
let show_evaluation_environment = pp_to_string pp_evaluation_environment;;


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
  | End_clause
[@@deriving ord, eq, to_yojson]
;;

let rec matches v p =
  match v,p with
  | _,Any_pattern -> true
  | Value_function _,Fun_pattern
  | Value_int _,Int_pattern ->
    true
  | Value_bool actual_boolean,Bool_pattern pattern_boolean ->
    actual_boolean = pattern_boolean
  | _ -> false
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


let rec string_of_annotated_clause cl : string =
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
  | Enter_clause(param, arg, context_clause) ->
    "Enter clause " ^ (string_of_var param) ^ " " ^ (string_of_var arg) ^ " " ^ string_of_annotated_clause (Unannotated_clause(context_clause))
  | Exit_clause(original_program_point, new_program_point, context_clause) ->
    "Exit clause " ^ (string_of_var original_program_point) ^ " " ^ (string_of_var new_program_point) ^ " " ^ string_of_annotated_clause (Unannotated_clause(context_clause))
  | Start_clause ->
    "Start of program"
  | End_clause ->
    "End of program"
;;

let print_graph graph : unit =
  print_endline "Graph:";
  Hashtbl.iter (fun x -> fun y -> print_endline ((string_of_annotated_clause x) ^ ", " ^ (string_of_annotated_clause y))) graph
;;
