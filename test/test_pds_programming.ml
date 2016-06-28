open OUnit2;;
open Batteries;;
open Pds_programming;;

let dummy_test _ =
  assert_equal 4 [%dummy 4]
;;

let double_test _ =
  let x =
    let%double y = 4 in y + 3
  in
  assert_equal 11 x
;;

let freevars_xy _ =
  let
    result = freevars String_set.empty [%expr x+y]
  in
  assert_equal ["x";"y"] (String_set.to_list result)

let tests = "Pds_programming" >::: [
    "dummy test" >:: dummy_test
  ]
;;
