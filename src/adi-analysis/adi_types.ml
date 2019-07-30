(** Defines core types for this analysis. *)

open Odefa_ast;;
open Odefa_abstract_ast;;

open Ast;;
open Abstract_ast;;

module type Context_model =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val empty : t
  val push : abstract_clause -> t -> t
  val pp : Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Safe.t
  val show : t -> string
  val name : string
end;;

module type Specification =
sig
  module C : Context_model
end;;

module type Analysis =
sig
  (** The specification for this analysis. *)
  module S : Specification
  (** A name for this analysis. *)
  val name : string
  (** The type of an analysis structure. *)
  type analysis
  (** Performs an analysis on the provided expression. *)
  val analyze : expr -> analysis
  (** Given an analysis, looks up the values of a particular variable in the
      empty context. *)
  val values_of_variable : abstract_var -> analysis -> Abs_value_set.t
  (** Given an analysis, looks up the values of a particular variable with a
      particular calling stack. *)
  val contextual_values_of_variable :
    abstract_var -> S.C.t -> analysis -> Abs_value_set.t
end;;
