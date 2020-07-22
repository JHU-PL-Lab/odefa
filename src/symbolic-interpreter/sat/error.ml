open Batteries;;

open Odefa_ast;;
open Ast;;
open Ast_pp;;

open Constraint;;

type error_binop = {
  err_binop_ident : ident;
  err_binop_operation : binary_operator;
  err_binop_left_val : value_source;
  err_binop_right_val : value_source;
}
;;

type error_match = {
  err_match_ident : ident;
  err_match_value : value_source;
  err_match_expected_type : type_sig;
  err_match_actual_type : type_sig;
}
;;

type error =
  | Error_binop of error_binop
  | Error_match of error_match
;;

module type Error_tree = sig
  type t;;
  val singleton : error -> t;;
  val add_and : t option -> t option -> t option;;
  val add_or : t option -> t option -> t option;;
  val tree_from_error_list : t list -> t;;
  val to_string : t -> string;;
end;;

module Error_tree : Error_tree = struct
  type t =
    | Node of t * t
    | Error of error
  ;;

  let singleton error = Error error;;

  let add_and et1_opt et2_opt =
    match (et1_opt, et2_opt) with
    | (Some et1, Some et2) -> Some (Node (et1, et2))
    | (Some et1, None) -> Some et1
    | (None, Some et2) -> Some et2
    | (None, None) -> None
  ;;

  let add_or et1_opt et2_opt =
    match (et1_opt, et2_opt) with
    | (Some et1, Some et2) -> Some (Node (et1, et2))
    | (_, _) -> None
  ;;

  let tree_from_error_list err_trees =
    let rec add_to_tree err_trees =
      match err_trees with
      | [err_tree] -> err_tree
      | err_tree :: err_trees' -> Node (err_tree, add_to_tree err_trees')
      | [] -> raise @@ Jhupllib.Utils.Invariant_failure "Missed base case"
    in
    add_to_tree err_trees
  ;;

  let rec flatten_tree error_tree =
    match error_tree with
    | Node (et1, et2) -> (flatten_tree et1) @ (flatten_tree et2)
    | Error error -> [error]
  ;;

  let error_to_string error =
    match error with
    | Error_binop binop_err ->
      begin
        "* Operation : " ^
        (show_value_source binop_err.err_binop_left_val) ^ " " ^
        (show_binary_operator binop_err.err_binop_operation) ^ " " ^
        (show_value_source binop_err.err_binop_right_val)
      end
    | Error_match match_err ->
      begin
        "* Value :    " ^ (show_value_source match_err.err_match_value) ^ " " ^
        "* Expected : " ^ (show_type_sig match_err.err_match_expected_type) ^ " " ^
        "* Actual :   " ^ (show_type_sig match_err.err_match_actual_type)
      end
  ;;

  let to_string error_tree =
    let error_list = flatten_tree error_tree in
    let string_list = List.map error_to_string error_list in
    String.join "\n" string_list
end;;