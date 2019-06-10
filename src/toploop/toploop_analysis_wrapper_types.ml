(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Odefa_ast;;
open Odefa_abstract_ast;;
(* open Odefa_ddpa;; *)

open Abstract_ast;;
open Ast;;
(* open Ddpa_analysis_logging;; *)
(* open Ddpa_context_stack;; *)

module type Wrapper_context = sig
  type t;;
  val push: (abstract_clause) -> t -> t;;
  val empty: t;;

end;;

module type Analysis_wrapper = sig
  type analysis

  module C : Wrapper_context;;

  val name : string;;

  type logging_config;;

  val create_analysis :
    ?logging_config:(logging_config option) -> expr -> analysis

  val values_of_variable_from :
    abstract_var -> annotated_clause -> analysis -> Abs_filtered_value_set.t

  val contextual_values_of_variable_from :
    abstract_var -> annotated_clause -> C.t -> analysis -> Abs_filtered_value_set.t

  val expression_of : analysis -> expr

  val pp_analysis : Format.formatter -> analysis -> unit
  val show_analysis : analysis -> string

  val get_size : analysis -> int * int * int * int * int

end;;
