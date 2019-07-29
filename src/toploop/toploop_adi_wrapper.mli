(* This module provides a functor that acts as a wrapper for DDPA analyses.
   The data type for this module stores the analysis in a ref cell so that
   successive calls to analysis functions gather and store information without
   bothering the caller with tracking the changes to the analysis as it grows.
*)

open Odefa_adi;;

open Toploop_analysis_wrapper_types;;

module Make : functor (A : Adi_types.Analysis) ->
  (Analysis_wrapper with type logging_config = unit);;
