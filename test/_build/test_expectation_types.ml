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

type expectation =
  | Expect_evaluate
  | Expect_stuck
  | Expect_well_formed
  | Expect_ill_formed
  | Expect_analysis_inconsistency_at of lookup_var
  | Expect_analysis_no_inconsistencies
;;

type analysis_expectation =
    Analysis_Expectation of query list * analysis_task list * result list
;;

type expectation_file =
  | Expectations of analysis_expectation option * expectation list
;;

(* here are modules for (query list * analysis_task list) that contains the
   cartesian product *)
(* TODO: include a pretty printer if we need one? *)
module Query_analysis_task =
struct
  type t = (query * analysis_task) [@@deriving eq, ord];;
  let compare = compare;;
end;;

module QA_set = Set.Make(Query_analysis_task);;
