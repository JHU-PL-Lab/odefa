open Jhupllib;;
open OUnit2;;
open Batteries;;
open Odefa_toploop;;
open Nondeterminism;;

open String_utils;;
open Toploop_types;;
open Test_expectation_types;;

(* Function that creates a list of checklist_items given expectations.
   (checklist_items type needed to be able to store both kinds of expectations)

   Params:
    gen_expects - list of general expectations
    analysis_expects - expectations pertaining to analyses

   Return:
    type checklist_items list
      a list that incorporates both kinds of expectations
*)
let make_checklist (gen_expects : expectation list)
    (analysis_expects : analysis_expectation)
  : (checklist_items list) =
  let gen_items =
    List.map
      (fun gen_expect ->
         (match gen_expect with
         | Expect_evaluate -> CLExpect_evaluate
         | Expect_stuck -> CLExpect_stuck
         | Expect_well_formed -> CLExpect_well_formed
         | Expect_ill_formed -> CLExpect_ill_formed)
      )
      gen_expects
  in
  let specific_items =
    let Analysis_Expectation(_, _, r_list, c_list) = analysis_expects in
    let res_items =
      List.map (fun res -> CLExpect_result res) r_list
    in
    let cons_items =
      List.map (fun cons -> CLExpect_consistency cons) c_list
    in
    res_items @ cons_items
  in
  gen_items @ specific_items
;;

(* Function that creates the AQ_Set (set of (analysis_task * query)) containing
   the cartesian product of the inputted analysis_task list and query list.
*)
let aq_set_creation (a_list : analysis_task list) (q_list : query list)
  :  AQ_set.t =
  let aq_monad =
    (let open Nondeterminism_monad in
     let%bind a = pick_enum @@ List.enum a_list in
     let%bind q = pick_enum @@ List.enum q_list in
     return (a, q))
  in
  let aq_list = List.of_enum (Nondeterminism_monad.enum aq_monad) in
  AQ_set.of_list aq_list
;;

(* Function that takes a list of analysis_consistency_expectation and
   creates a map that binds the analysis_task to the corresponding
   consistency.
*)
let ac_tuple_list_to_dict (tuple_list : analysis_consistency_expectation list)
  : consistency AC_Map.t =
  let mapper =
    (fun ac_dict -> fun ac_tuple ->
       let (a_task, c_list) = ac_tuple in
       AC_Map.add a_task c_list ac_dict
    )
  in
  List.fold_left mapper AC_Map.empty tuple_list
;;

let natural_compare_seq_returns_0_for_empty_list _ =
  assert_equal 0 (Utils.natural_compare_seq [])

let string_of_list_list pp lst =
  string_of_list (string_of_list pp) lst
;;

let cartesian_product_tests =
  let do_test lst1 lst2 _ =
    assert_equal (Utils.cartesian_product_of_list lst1) lst2
  in
  let make_test (lst1,lst2) =
    let test_name = "cartesian_product_of_list " ^
                    string_of_list_list string_of_int lst1 ^ " = " ^
                    string_of_list_list string_of_int lst2
    in
    test_name >:: do_test lst1 lst2
  in
  List.map make_test
    [ ( []
      , [[]]
      )
    ; ( [[1]]
      , [[1]]
      )
    ; ( [[1;2]]
      , [[1];[2]]
      )
    ; ( [[1;2];[3]]
      , [[1;3];[2;3]]
      )
    ; ( [[1;2];[3];[4;5;6]]
      , [[1;3;4];[2;3;4];[1;3;5];[2;3;5];[1;3;6];[2;3;6]]
      )
    ; ( [[1;2];[];[4;5;6]]
      , []
      )
    ]

let tests = "Utils" >::: [
    "natural_compare_seq returns 0 for empty list" >:: natural_compare_seq_returns_0_for_empty_list;
  ] @
    cartesian_product_tests
;;
