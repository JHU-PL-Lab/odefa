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
  let pushes cs x =
    cs
    |> List.fold_left
      (fun a e -> push e a) x
  ;;
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

let analysis_n_element_collapsing_stack_test =
  "analysis_n_element_collapsing_stack_test" >:: fun _ ->
    let module Spec = struct let size = 3 end in
    let module C3DDPA = Analysis_n_element_collapsing_stack.Make(Spec) in
    let module T = StackTester(C3DDPA) in
    let x = dummy_clause "x" in
    let y = dummy_clause "y" in
    let z = dummy_clause "z" in
    let w = dummy_clause "w" in
    assert_bool "2 vs. 3 pushes should be the same"
      (T.eq (T.pushes [x;x] T.empty) (T.pushes [x;x;x] T.empty));
    assert_bool "1 vs. 2 pushes should not be the same"
      (not @@ T.eq (T.pushes [x] T.empty) (T.pushes [x;x] T.empty));
    assert_bool "xyx should be the same as yxy"
      (T.eq (T.pushes [x;y;x] T.empty) (T.pushes [y;x;y] T.empty));
    assert_bool "xyzw should be the same as yzw"
      (T.eq (T.pushes [x;y;z;w] T.empty) (T.pushes [y;z;w] T.empty))
;;

let tests = "Test_context_stacks" >:::
            [ unit_stack_test
            ; singleton_stack_test
            ; analysis_n_element_collapsing_stack_test
            ]
;;
