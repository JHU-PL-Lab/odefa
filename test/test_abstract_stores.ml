open Batteries;;
open Jhupllib;;
open OUnit2;;

open Core_ast;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;
open Pp_utils;;

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
    assert_equal ~printer:(pp_to_string pp_abstract_value)
      (Abs_value_bool true) (store_read s3);
    store_enum s3
    |> Enum.iter
      (fun (rx,v) ->
         let (_,t) = destruct_relative_trace_var rx in
         if is_partial_trace t then
           ()
         else
           assert_failure @@
           Printf.sprintf "Expected partial trace in every mapping but found %s"
             ((pp_to_string @@ pp_tuple Relative_trace_var.pp pp_abstract_value)
                (rx,v))
      )
;;

let tests =
  "Test_abstract_stores" >:::
  [ test_serial_join_with_imprecise
  ]
;;
