(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Core_ast;;
open Ddpa_analysis_logging;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;

module type DDPA_wrapper = sig
  type analysis

  val create_analysis :
    ?logging_config:(ddpa_analysis_logging_config option) -> expr -> analysis

  val values_of_variable_from :
    abstract_var -> annotated_clause -> analysis -> Abstract_store_set.t

  val expression_of : analysis -> expr

  val pp_analysis : Format.formatter -> analysis -> unit
  val show_analysis : analysis -> string

  val get_size : analysis -> int * int * int * int * int
end;;
