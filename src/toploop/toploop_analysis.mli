(* This module provides an error checker for Odefa programs which relies upon
   DDPA to find problems such as non-function application, non-record
   projection, etc. *)

open Toploop_analysis_types;;
open Toploop_analysis_wrapper_types;;

module Make :
  functor (Analysis_wrapper : Analysis_wrapper) ->
    Analysis_sig with module Analysis_wrapper = Analysis_wrapper
;;
