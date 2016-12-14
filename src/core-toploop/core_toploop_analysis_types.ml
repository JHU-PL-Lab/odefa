(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Batteries;;

open Core_ast;;
open Core_ast_pp;;
open Core_toploop_ddpa_wrapper_types;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;

type error =
  | Application_of_non_function of
      abstract_var * abstract_var * Abstract_store.t * Abstract_store_set.t
  (** Represents the application of a non-function value.  The arguments
      are the variable identifying the call site clause, the invoked variable,
      and the abstract non-function value which appeared at the call site. *)
  | Projection_of_non_record of abstract_var * abstract_var * Abstract_store.t
  (** Represents the projection of a label from a non-record value.  The
      arguments are the variable identifying the clause containing the
      projection, the record variable, and the abstract value from which the
      projection occurred. *)
  | Projection_of_absent_label of
      abstract_var * abstract_var * Abstract_store.t * ident
  (** Represents the projection of a label from a record value which does not
      have that label.  The arguments are the variable identifying the clause
      containing the projection, the record variable, the abstract value from
      which the projection occurred, and the ident we failed to project. *)
  | Deref_of_non_ref of abstract_var * abstract_var * Abstract_store.t
  (** Represents the dereference of a non-ref value.  The arguments are the
      variable identifying the clause where the assignment occurred, the
      dereferenced variable, and the abstract non-cell value which appeared
      there. *)
  | Update_of_non_ref of abstract_var * abstract_var * Abstract_store.t
  (** Represents the cell-set update of a non-ref value.  The arguments are
      the variable identifying the clause where the assignment occurred, the
      updated variable, and the abstract non-cell value which appeared
      there. *)
  | Invalid_binary_operation of
      abstract_var * binary_operator * abstract_var * Abstract_store.t *
      abstract_var * Abstract_store.t
  (** Represents invalid use of a binary operator.  The arguments are, in order,
      - The variable identifying the clause where the assignment occurred.
      - The binary operator appearing in the clause.
      - The variable for the first operand.
      - A possible value of the first operand.
      - The variable for the second operand.
      - A possible value of the second operand.
  *)
  | Invalid_unary_operation of
      abstract_var * unary_operator * abstract_var * Abstract_store.t
  (** Represents invalid use of a unary operator.  The arguments are, in order,
      - The variable identifying the clause where the assignment occurred.
      - The unary operator appearing in the clause.
      - The variable for the operand.
      - A possible value of the operand.
  *)
  | Invalid_indexing_subject of
      abstract_var * abstract_var * Abstract_store.t
  (** Represents the indexing of a non-indexable subject.  The arguments are
      the variable identifying the indexing clause, the variable of the indexing
      subject, and a possible value of the indexing subject. *)
  | Invalid_indexing_argument of
      abstract_var * abstract_var * Abstract_store.t
  (** Represents an invalid indexing argument.  The arguments are the
      variable identifying the indexing clause, the variable of the index,
      and a possible value of the index. *)
  [@@deriving eq, ord, show]
;;

module Error_ord =
struct
  type t = error
  let compare = compare_error
end;;

module Error_set = Set.Make(Error_ord);;

module type Analysis_sig = sig
  module DDPA_wrapper : DDPA_wrapper
  val find_errors : DDPA_wrapper.analysis -> error Enum.t
end;;
