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

let unit_stack_test =
  "unit_stack_test" >:: fun _ ->
    let open Analysis_unit_stack in
    let s's = Stack.push (dummy_clause "x") Stack.empty in
    s's |> Enum.iter (fun s' ->
        assert_bool "Unit stack push should be a nop"
          (Stack.compare Stack.empty s' = 0)
      )
;;

let singleton_stack_test =
  "singleton_stack_test" >:: fun _ ->
    let open Analysis_single_element_stack in
    let sx = Stack.push (dummy_clause "x") Stack.empty in
    let sy = Stack.push (dummy_clause "y") Stack.empty in
    let sxy = Enum.concat @@
      Enum.map (Stack.push (dummy_clause "y")) (Enum.clone sx)
    in
    Enum.cartesian_product sy sxy |> Enum.iter
      (fun (s1,s2) ->
         assert_bool "Last push should dictate singleton stack"
           (Stack.compare s1 s2 = 0)
      )
;;

let tests = "Test_context_stacks" >:::
            [ unit_stack_test
            ; singleton_stack_test
            ]
;;
