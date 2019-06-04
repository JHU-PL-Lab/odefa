(* This module provides a functor that acts as a wrapper for DDPA analyses.
   The data type for this module stores the analysis in a ref cell so that
   successive calls to analysis functions gather and store information without
   bothering the caller with tracking the changes to the analysis as it grows.
*)

open Odefa_ddpa;;

open Ddpa_analysis_logging;;
open Toploop_analysis_wrapper_types;;

module Make : functor (A : Ddpa_analysis.Analysis_sig) ->
  (Analysis_wrapper with type logging_config = ddpa_analysis_logging_config);;
