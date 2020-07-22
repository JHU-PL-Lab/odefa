open Odefa_ast;;
open Ast;;

open Constraint;;

type error_binop = {
  (** The identifier of the binop clause. *)
  err_binop_ident : ident;

  (** The operator (e.g. +, -, and, or, ==, etc. *)
  err_binop_operation : binary_operator;

  (** The value of the left side of the binop. *)
  err_binop_left_val : value_source;

  (** The value of the right side of the binop. *)
  err_binop_right_val : value_source;
}
;;

type error_match = {
  (** The identifier of the pattern match clause. *)
  err_match_ident : ident;

  (** The value of the symbol that is being matched. *)
  err_match_value : value_source;

  (** The expected type, according to the pattern. *)
  err_match_expected_type : type_sig;

  (** The actual type of the symbol being matched. *)
  err_match_actual_type : type_sig;
}
;;

type error =
  | Error_binop of error_binop
  | Error_match of error_match
;;

module type Error_tree = sig
  type t;;

  (** Create an error tree that contains a single error leaf node. *)
  val singleton : error -> t;;

  (** Merge two error trees as if they are part of an AND operation.
      In an AND operation, all values must be true for the op to return true.
      Therefore if one error has a false value, the error tree is false. *)
  val add_and : t option -> t option -> t option;;

  (** Merge two error trees as if they are part of an OR operation.
      In an OR operation, only one value needs to be true for the op to be true
      so only when all errors have a false value can the error tree be false. *)
  val add_or : t option -> t option -> t option;;

  (** Create an error tree from a list of error trees (assuming all top-level
      predicates have a false value). *)
  val tree_from_error_list : t list -> t;;

  (** String representation of the error tree. *)
  val to_string : t -> string;;
end;;

module Error_tree : Error_tree;;