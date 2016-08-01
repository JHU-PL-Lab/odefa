open OUnit2;;
open Continuation_transform;;
open Ocaml_a_translator;;

let ident_test _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr x] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr x] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal expected actual
;;

let constant_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr 4] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr 4] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal expected actual
;;

let constant_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr 'a'] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr 'a'] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal expected actual
;;

let read_test _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr [%read]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back = Cont_handler ([%pat? Part0], [%expr next_token]) in
  let expected_others = Handler_set.empty in
  let expected_hgroup = Some {back = expected_back; others = expected_others} in
  let expected_start = [%expr Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

(* let result_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr [%result 4]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list = [] in
  let expected_start = [%expr Result 4] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;;

let result_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list = [] in
  let expected_start = [%expr Result x] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;;

let let_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = 4 in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list = [] in
  let expected_start = [%expr let x = 4 in Result x] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;;

let let_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = [%read] in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list = [([%pat? Part0], [%expr let x = next_token in Result x])] in
  let expected_start = [%expr Part0] in
  let expected = (expected_list, expected_start)
    in
  assert_equal expected actual
;;

let let_test_3 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = 3 in let y = [%read] in [%result (x, y)]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list =
    [([%pat? Part0], [%expr let y = next_token in Result (x, y)])] in
  let expected_start = [%expr let x = 3 in Part0] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;;

let let_test_4 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = [%read]
                 in let y = [%read]
                 in [%result (x,y)]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list =
    [ (
      [%pat? Part0], [%expr let x = next_token in Part1]
    );
      (
        [%pat? Part1], [%expr let y = next_token in Result (x, y)]
      )
    ] in
  let expected_start = [%expr Part0] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;;

let tuple_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = (2, 3) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list = [] in
  let expected_start = [%expr let x =
                                (let var0 = 2 in
                                let var1 = 3 in
                                 (var0, var1)) in Result x] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;;

let tuple_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = (3, [%read]) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list = [([%pat? Part0],
                        [%expr let x = (let var1 = next_token
                                        in (var0, var1)) in Result x])] in
  let expected_start = [%expr let var0 = 3 in Part0] in
  let expected = (expected_list, expected_start)
    in
  assert_equal expected actual
;;

let tuple_test_3 _ =
  let context1 = Ocaml_a_translator.new_context() in
  let e = [%expr let x = ([%read], 3) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list =
    [([%pat? Part0],
      [%expr let x =
               (let var0 = next_token in
                let var1 = 3 in
                (var0, var1))
             in Result x])] in
  let expected_start = [%expr Part0] in
  let expected = (expected_list, expected_start)
    in
  assert_equal expected actual
;;

let tuple_test_4 _ =
  let context1 = Ocaml_a_translator.new_context() in
  let e = [%expr let x = ([%read], [%read]) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_list =
    [([%pat? Part0],
      [%expr let x = (let var0 = next_token in Part1)
             in Result x])
    ;
     ([%pat? Part1],
      [%expr let var1 = next_token in (var0, var1)])
     ] in
  let expected_start = [%expr Part0] in
  let expected = (expected_list, expected_start)
  in
  assert_equal expected actual
;; *)

(*TODO: test construct*)

(*TODO: test apply*)

(*TODO: test record*)

(*TODO: test field*)

let tests = "Continuation_transform" >::: [

    "ident test" >:: ident_test;
    "constant test 1" >:: constant_test_1;
    "constant test 2" >:: constant_test_2;
    "read test" >:: read_test;
    (* "result test 1" >:: result_test_1;
    "result test 2" >:: result_test_2;
    "let test 1" >:: let_test_1;
    "let test 2" >:: let_test_2;
    "let test 3" >:: let_test_3;
    "let test 4" >:: let_test_4;
    "tuple test 1" >:: tuple_test_1;
    "tuple test 2" >:: tuple_test_2;
    "tuple test 3" >:: tuple_test_3;
       "tuple test 4" >:: tuple_test_4;*)

  ]
;;
