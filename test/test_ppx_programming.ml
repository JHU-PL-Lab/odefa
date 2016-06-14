open OUnit2

let dummy_test _ =
  assert_equal 4 [%dummy 4]
;;

let tests = "Ppx_programming" >::: [
    "dummy test" >:: dummy_test
  ]
;;
