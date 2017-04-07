open Batteries;;
open Jhupllib;;
open OUnit2;;

open Core_ast;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;

module Delta1StoreOps = Ops.Make(
  struct
    let maximum_trace_length = 1;;
  end);;

let test_serial_join_with_imprecise =
  "test_serial_join_with_imprecise" >:: fun _ ->
    let c1 = Abs_clause(Abs_var(Ident "c1"), Abs_value_body Abs_value_int) in
    let c2 = Abs_clause(Abs_var(Ident "c2"), Abs_value_body Abs_value_int) in
    let s1 =
      Delta1StoreOps.store_singleton (Abs_var(Ident("x"))) (Abs_value_bool true)
    in
    let s2 =
      Delta1StoreOps.store_singleton (Abs_var(Ident("y"))) Abs_value_int
      |> flip Delta1StoreOps.store_suffix_trace_part (Trace_enter c1)
      |> Option.get
      |> flip Delta1StoreOps.store_suffix_trace_part (Trace_enter c2)
      |> Option.get
    in
    let s3 = Option.get @@ Delta1StoreOps.serial_store_join s1 s2 in
    assert_equal ~printer:(Pp_utils.pp_to_string pp_abstract_value)
      (Abs_value_bool true) (store_read s3);
    assert_equal
      ~printer:(Pp_utils.pp_to_string @@ Pp_utils.pp_list @@
                Pp_utils.pp_tuple Relative_trace_var.pp pp_abstract_value)
      []
      (List.of_enum @@ store_enum s3)
;;

let tests =
  "Test_abstract_stores" >:::
  [ test_serial_join_with_imprecise
  ]
;;
