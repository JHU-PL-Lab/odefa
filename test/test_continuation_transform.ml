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
  let expected_back =
    {h_pat = [%pat? Part0];
     h_exp = [%expr next_token];
     h_type = Cont_handler} in
  let expected_others = Handler_set.empty in
  let expected_hgroup = Some {back = expected_back; others = expected_others} in
  let expected_start = [%expr Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let result_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr [%result 4]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr Result 4] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let result_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr Result x] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let let_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = 4 in x] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr let var0 = 4 in let x = var0 in x] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let let_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = [%read] in x] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back =
    {h_pat = [%pat? Part0];
     h_exp = [%expr let var0 = next_token in let x = var0 in x];
     h_type = Cont_handler} in
  let expected_hset = Handler_set.empty in
  let expected_hgroup = Some {back = expected_back; others = expected_hset} in
  let expected_start = [%expr Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let let_test_3 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = 3 in let y = [%read] in [%result (x, y)]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back =
    { h_pat = [%pat? Part0];
      h_exp = [%expr let var0 = next_token in let y = var0 in Result (x, y)];
      h_type = Cont_handler} in
  let expected_hset = Handler_set.empty in
  let expected_hgroup = Some {back = expected_back; others = expected_hset} in
  let expected_start = [%expr let var1 = 3 in let x = var1 in Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let let_test_4 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = [%read]
                 in let y = [%read]
                 in [%result (x,y)]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hset =
    Handler_set.singleton
      ({ h_pat = [%pat? Part0];
         h_exp = [%expr let var1 = next_token in let x = var1 in Part1];
         h_type = Cont_handler}) in
  let expected_back =
    {h_pat = [%pat? Part1];
     h_exp = [%expr let var0 = next_token in let y = var0 in Result (x, y)];
     h_type = Cont_handler}  in
  let expected_hgroup = Some {back = expected_back; others = expected_hset} in
  let expected_start = [%expr Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let tuple_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = (2, 3) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr let var2 =
                                (let var0 = 2 in
                                 let var1 = 3 in
                                 (var0, var1)) in
                              let x = var2 in
                              Result x] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let tuple_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr let x = (3, [%read]) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back =
    {h_pat = [%pat? Part0];
     h_exp = [%expr let var2 =
                      let var1 = next_token in
                      (var0, var1) in
                    let x = var2 in Result x];
     h_type = Cont_handler} in
  let expected_others = Handler_set.empty in
  let expected_hgroup = Some {back = expected_back; others = expected_others} in
  let expected_start = [%expr let var0 = 3 in Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let tuple_test_3 _ =
  let context1 = Ocaml_a_translator.new_context() in
  let e = [%expr let x = ([%read], 3) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back =
    {h_pat = [%pat? Part0];
     h_exp = [%expr let var2 =
                      (let var0 = next_token in
                       let var1 = 3 in
                       (var0, var1))
                    in let x = var2 in
                    Result x];
     h_type = Cont_handler} in
  let expected_others = Handler_set.empty in
  let expected_hgroup =
    Some {back = expected_back; others = expected_others} in
  let expected_start = [%expr Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let tuple_test_4 _ =
  let context1 = Ocaml_a_translator.new_context() in
  let e = [%expr let x = ([%read], [%read]) in [%result x]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back =
    {h_pat = [%pat? Part1];
     h_exp = [%expr let var2 =
                           (let var1 = next_token in
                            (var0, var1)) in
                         let x = var2 in
                    Result x];
     h_type = Cont_handler} in
  let h_elt =
    {h_pat = [%pat? Part0];
     h_exp = [%expr let var0 = next_token in Part1];
     h_type = Cont_handler} in
  let hset = Handler_set.singleton h_elt in
  let expected_hgroup = Some {back = expected_back; others = hset} in
  let expected_start = [%expr Part0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

(*TODO: test construct*)

(*TODO: test apply*)

(*TODO: test record*)

(*TODO: test field*)

let ifthenelse_test_1 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr if x then 1 else 0] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_hgroup = None in
  let expected_start = [%expr let var0 = x in if var0 then 1 else 0] in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected actual
;;

let ifthenelse_test_2 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr if x then [%read] else 0] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let acutal = continuation_transform a_e context2 in
  let expected_back = {h_pat = [%pat? Goto2 __varct__0];
                       h_exp = [%expr __varct__0];
                       h_type = Goto_handler} in
  let h_elt_1 = {h_pat = [%pat? Part0];
                 h_exp = [%expr Goto2 next_token];
                 h_type = Cont_handler} in
  let h_elt_2 = {h_pat = [%pat? Goto0];
                 h_exp = [%expr Part0];
                 h_type = Goto_handler} in
  let h_elt_3 = {h_pat = [%pat? Goto1];
                 h_exp = [%expr Goto2 0];
                 h_type = Goto_handler} in
  let expected_others =
    Handler_set.singleton h_elt_1
    |> Handler_set.add h_elt_2
    |> Handler_set.add h_elt_3
  in
  let expected_start = [%expr let var0 = x in if var0 then Goto0 else Goto1] in
  let expected_hgroup = Some {back = expected_back;
                         others = expected_others;} in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~printer:show_continuation_transform_result expected acutal
;;

let ifthenelse_test_3 _ =
  let context1 = Ocaml_a_translator.new_context () in
  let e = [%expr if x then 0 else [%read]] in
  let a_e = a_translator e context1 in
  let context2 = Continuation_transform.new_context () in
  let actual = continuation_transform a_e context2 in
  let expected_back = {h_pat = [%pat? Goto2 __varct__0];
                       h_exp = [%expr __varct__0];
                       h_type = Goto_handler} in
  let h_elt_1 = {h_pat = [%pat? Part0];
                 h_exp = [%expr Goto2 next_token];
                 h_type = Cont_handler} in
  let h_elt_2 = {h_pat = [%pat? Goto1];
                 h_exp = [%expr Part0];
                 h_type = Goto_handler} in
  let h_elt_3 = {h_pat = [%pat? Goto0];
                 h_exp = [%expr Goto2 0];
                 h_type = Goto_handler} in
  let expected_others =
    Handler_set.singleton h_elt_1
    |> Handler_set.add h_elt_2
    |> Handler_set.add h_elt_3
  in
  let expected_start = [%expr let var0 = x in if var0 then Goto0 else Goto1] in
  let expected_hgroup = Some {back = expected_back;
                              others = expected_others} in
  let expected = (expected_hgroup, expected_start)
  in
  assert_equal ~cmp:equal_continuation_transform_result ~printer:show_continuation_transform_result expected actual
;;

let tests = "Continuation_transform" >::: [

    "ident test" >:: ident_test;
    "constant test 1" >:: constant_test_1;
    "constant test 2" >:: constant_test_2;
    "read test" >:: read_test;
    "result test 1" >:: result_test_1;
    "result test 2" >:: result_test_2;
    "let test 1" >:: let_test_1;
    "let test 2" >:: let_test_2;
    "let test 3" >:: let_test_3;
    "let test 4" >:: let_test_4;
    "tuple test 1" >:: tuple_test_1;
    "tuple test 2" >:: tuple_test_2;
    "tuple test 3" >:: tuple_test_3;
    "tuple test 4" >:: tuple_test_4;
    "ifthenelse test 1" >:: ifthenelse_test_1;
    "ifthenelse test 2" >:: ifthenelse_test_2;
    "ifthenelse test 3" >:: ifthenelse_test_3;

  ]
;;
