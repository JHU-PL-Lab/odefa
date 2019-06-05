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

open Batteries;;
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

type lookup_var = Var of string;;

type graph_position =
  | ProgramPoint of string
  | START
  | END
;;

type context = lookup_var list;;

type analysis =
  | DDPA of int
  | PLUME of int
  | SPLUME
  | OSPLUME
;;

(* TODO: Unify this type with the one in toploops? *)
type query =
  | Query of lookup_var * graph_position option * context option
;;

type result_string = ResultString of string
;;

type result = Result of analysis * query * result_string
;;

type expectation =
  | Expect_evaluate
  | Expect_stuck
  | Expect_well_formed
  | Expect_ill_formed
  | Expect_analysis_inconsistency_at of var
  | Expect_analysis_no_inconsistencies
;;

type analysis_expectation =
    Analysis_Expectation of query list * analysis list * result list
;;

type expectation_file =
  | Expectations of analysis_expectation option * expectation list
;;
