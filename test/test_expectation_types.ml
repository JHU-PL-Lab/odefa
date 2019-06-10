(**
   This test module will load a series of test files from the test sources
   directory and execute them one by one.

   Each file is expected to contain a comment describing the expected test
   result.  The comment should be of one of the following forms:

   - [EXPECT-EVALUATE] (which requires that the code evaluates to completion)
   - [EXPECT-STUCK] (which requires that the code gets stuck)
     FIXME: update this documentation
*)

(* FIXME: purge the term "inconsistency" *)
(* open Batteries;; *)
(* open Jhupllib;;
   open OUnit2;;

   open Odefa_ast;;
   open Odefa_ddpa;;
   open Odefa_parser;;
   open Odefa_toploop;;

   open Ast;;
   open Ast_pp;;
   open Ast_wellformedness;;
   open Toploop_options;;
   open Toploop_types;;
   open Ddpa_abstract_ast;;
   open String_utils;; *)
open Odefa_toploop;;
open Toploop_types;;



type result_string = ResultString of string
;;

type result = Result of analysis_task * query * result_string
;;

type inconsistency = Expect_analysis_inconsistency_at of lookup_var;;

type consistency =
  | Expect_analysis_inconsistencies of inconsistency list
  | Expect_analysis_no_inconsistencies
;;

type analysis_consistency_expectation = analysis_task * consistency;;

type expectation =
  | Expect_evaluate
  | Expect_stuck
  | Expect_well_formed
  | Expect_ill_formed
;;

type checklist =
  | CLExpect_evaluate
  | Expect_stuck
  | Expect_well_formed
  | Expect_ill_formed
  | CLExpect_result of result
  | CLExpect_consistency of analysis_consistency_expectation
;;

type analysis_expectation =
    Analysis_Expectation of query list * analysis_task list * result list * analysis_consistency_expectation list
;;

type expectation_file =
  | Expectations of analysis_expectation option * expectation list
;;

(* here are modules for (query list * analysis_task list) that contains the
   cartesian product *)
(* TODO: include a pretty printer if we need one? *)
module Analysis_task_query =
struct
  type t = (analysis_task * query) [@@deriving eq, ord];;
  let compare = compare;;
end;;

module Analysis_task =
struct
  type t = analysis_task [@@deriving eq, ord];;
  let compare = compare;;
end;;

module AQ_set = Set.Make(Analysis_task_query);;

module AC_Map = Map.Make(Analysis_task);;
