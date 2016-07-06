open OUnit2;;
open Continuation_transform;;

let nested_let_test_1 _ =
  let context = new_context () in
  let e = [%expr
    let x = [%read] in
    let y = [%read] in
    [%result x + y]
  ] in
  let expected_start = [%expr Part0] in
  let expected_list =
    [ ( [%pat? Part0]
      , [%expr let x = next_token in Part1]
      )
    ; ( [%pat? Part1]
      , [%expr let y = next_token in Result (x + y)]
      )
    ]
  in
  assert_equal (expected_list, expected_start) (continuation_transform e context)
;;

let nested_let_test_2 _ =
  let context = new_context () in
  let e = [%expr
    let x = 4 in
    let y = [%read] in
    [%result x + y]
  ] in
  let expected_start = [%expr let x = 4 in Part0] in
  let expected_list =
    [ ( [%pat? Part0]
      , [%expr let y = next_token in Result (x + y)]
      )
    ]
  in
  assert_equal (expected_list, expected_start) (continuation_transform e context)
;;

let tuple_test_1 _ =
  let context = new_context () in
  let e = [%expr (4, 3)] in
  let expected_start = [%expr (4,3)] in
  let expected_list = [] in
  assert_equal (expected_list, expected_start) (continuation_transform e context)
;;

(*not ready for this one yet!!*)
(* let nested_let_test_3 _ =
  let context = new_context () in
  let e = [%expr
    let x = 4 in
    let y = [%read] in
    let z = x + y in
    let a = [%read] in
    [%result a + z]
  ] in
  let expected_start = [%expr let x = 4 in Part0] in
  let expected_list =
    [ ( [%pat? Part0]
      , [%expr let y = next_token in let z = x + y in Part1]
      )
    ; ( [%pat? Part1]
      , [%expr let a = next_token in Result (a + z)]
      )
    ]
  in
  assert_equal (expected_list, expected_start) (continuation_transform e context)
   ;; *)

let tests = "Continuation_transform" >::: [

    "nested let test 1" >:: nested_let_test_1;
    "nested let test 2" >:: nested_let_test_2;
    (*"nested let test 3" >:: nested_let_test_3;*)
    "tuple test 1" >:: tuple_test_1;

  ]
;;
