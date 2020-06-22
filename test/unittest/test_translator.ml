open Batteries;;
(* open Jhupllib;; *)
open OUnit2;;

open Odefa_parser;;

open Odefa_natural;;
open On_to_odefa;;
open Translator_utils;;

open Odefa_ast;;
open Ast_pp;;

(* **** Scaffolding **** *)

exception Bug;;

let _tests_acc = ref [];;
let _add_test name testfn = _tests_acc := (name >:: testfn) :: !_tests_acc;;

let parse_odefa (str: string) : Ast.expr =
  let buf = Lexing.from_string str in
  let expr_opt = Generated_parser.delim_expr Generated_lexer.token buf in
  match expr_opt with
  | Some expr -> expr
  | None -> raise @@ Bug
;;

(* **** Tests **** *)

_add_test "pattern_variables" @@ fun _ ->
  let input = [
        On_ast.AnyPat;
        On_ast.IntPat;
        On_ast.BoolPat;
        On_ast.FunPat;
        On_ast.EmptyLstPat;
        (On_ast.VarPat (On_ast.Ident("x")))
      ]
  in
  let expected =
    [(On_ast.VarPat (On_ast.Ident "x"))]
  in
  let actual =
    List.filter
      (fun pattern ->
        not (On_ast.Ident_set.is_empty @@ pat_vars pattern))
      input
  in
  assert_equal expected actual
;;

(* TODO: Test desugaring *)

(* TODO: Test alphatizing *)

(* TODO: Test flattening *)

(* Test constraining *)

let calculate_actual (input: string) : Ast.expr =
  let open TranslationMonad in
  let expr = parse_odefa input in
  let Ast.Expr(clist) = expr in
  let tc = new_translation_context () in
  let new_clist = run tc (condition_clauses clist) in
  Ast.Expr(new_clist)
;;

let eliminate_tildes (str: string) : string =
  let re = Str.regexp "~\\([0-9]\\)" in
  Str.global_replace re "___\\1" str
;;

let test_exprs (input: string) (expected: string) : unit =
  let actual_expr = calculate_actual input in
  let actual_expr' =
    actual_expr
    |> show_expr
    |> eliminate_tildes
    |> parse_odefa
  in
  let expected_expr = parse_odefa @@ eliminate_tildes expected in
  assert_equal
    ~printer:show_expr
    ~cmp:(=)
    expected_expr
    actual_expr'
;;

_add_test "constrain_arithmetic" @@ fun _ ->
  let input =
    "a = input;
     b = input;
     binop = a + b;"
  in
  let expected =
    "a = input;
     b = input;
     m1~0 = a ~ int;
     m2~1 = b ~ int;
     m~2 = m1~0 and m2~1;
     constrain_binop~3 = m~2 ? (binop = a + b) : (ab~4 = abort);"
  in
  test_exprs input expected
;;

_add_test "constrain_boolean" @@ fun _ ->
  let input =
    "a = true;
     b = false;
     binop = a and b;"
  in
  let expected =
    "a = true;
     b = false;
     m1~0 = a ~ bool;
     m2~1 = b ~ bool;
     m~2 = m1~0 and m2~1;
     constrain_binop~3 = m~2 ? (binop = a and b) : (ab~4 = abort)"
  in
  test_exprs input expected
;;

_add_test "constrain_application" @@ fun _ ->
  let input =
    "f = fun x -> (ret = x);
     a = input;
     appl = f a;"
  in
  let expected =
    "f = fun x -> (ret = x);
     a = input;
     m~0 = f ~ fun;
     constrain_appl~1 = m~0 ? (appl = f a) : (ab~2 = abort)"
  in
  test_exprs input expected
;;

_add_test "constrain_projection" @@ fun _ ->
  let input =
    "a = input;
     r = {label=a};
     proj = r.label;"
  in
  let expected =
    "a = input;
     r = {label=a};
     m~0 = r ~ {label};
     constrain_proj~1 = m~0 ? (proj = r.label) : (ab~2 = abort)"
  in
  test_exprs input expected
;;

_add_test "constrain_conditional" @@ fun _ ->
  let input =
    "pred = true;
     cond = pred ? (res1 = true) : (res2 = false);"
  in
  let expected =
    "pred = true;
     m~0 = pred ~ bool;
     constrain_cond~1 = m~0 ? (cond = pred ? (res1 = true) : (res2 = false))
                            : (ab~2 = abort);"
  in
  test_exprs input expected
;;

_add_test "constrain_in_fun" @@ fun _ ->
  let input =
    "f = fun x -> (
       g = fun y -> (rety = x + y);
       retx = g);
     a = input;
     b = input;
     appl1 = f a;
     appl2 = appl1 b;"
  in
  let expected = 
    "f = fun x -> (
       g = fun y -> ( 
         m1~0 = x ~ int;
         m2~1 = y ~ int;
         m~2 = m1~0 and m2~1;
         constrain_binop~3 = m~2 ? (rety = x + y) : (ab~4 = abort));
       retx = g);
     a = input;
     b = input;
     m~5 = f ~ fun;
     constrain_appl~6 = m~5 ? (appl1 = f a;
                               m~7 = appl1 ~ fun;
                               constrain_appl~8 = m~7 ? (appl2 = appl1 b)
                                                      : (ab~9 = abort)
                          ) : (ab~10 = abort)"
  in
  test_exprs input expected
;;

(* **** For main test module **** *)

let tests = "natodefa translator tests" >::: List.rev !_tests_acc;;
