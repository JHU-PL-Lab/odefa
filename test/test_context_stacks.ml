(**
   This test module checks the behavior of the various context stack
   implementations.
*)

open Batteries;;
open OUnit2;;

open Ast;;
open Ddpa_graph;;

let dummy_var n = Var(Ident n, None);;

let dummy_clause n = Abs_clause(dummy_var n, Abs_value_body(Abs_value_int));;

module StackTester(C : Analysis_context_stack.Context_stack) =
struct
  module C = C;;
  let empty : C.t list = [C.empty];;
  let apply f x =
    x
    |> List.enum
    |> Enum.map f
    |> Enum.concat
    |> List.of_enum
  ;;

  let push c x = apply (C.push c) x;;
  let pop x = apply C.pop x;;
  let eq x y =
    Enum.cartesian_product (List.enum x) (List.enum y)
    |> Enum.for_all (fun (x,y) -> C.compare x y = 0)
  ;;
end;;

let unit_stack_test =
  "unit_stack_test" >:: fun _ ->
    let module T = StackTester(Analysis_unit_stack.Stack) in
    let ss = T.push (dummy_clause "x") T.empty in
    assert_bool "Unit stack push should be a nop" (T.eq ss T.empty)
;;

let singleton_stack_test =
  "singleton_stack_test" >:: fun _ ->
    let module T = StackTester(Analysis_single_element_stack.Stack) in
    let sx = T.push (dummy_clause "x") T.empty in
    let sy = T.push (dummy_clause "y") T.empty in
    let sxy = T.push (dummy_clause "y") sx in
    assert_bool "Last push should dictate singleton stack" (T.eq sxy sy)
;;

let tests = "Test_context_stacks" >:::
            [ unit_stack_test
            ; singleton_stack_test
            ]
;;
