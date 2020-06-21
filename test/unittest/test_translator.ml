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
        On_ast.TruePat;
        On_ast.FalsePat;
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

(* TODO: Test constraining *)

let format_expected expected_str =
  expected_str
  |> Str.global_replace (Str.regexp ";\n[ ]*") ";\n  "
  |> (fun s -> "\n  " ^ s ^ "\n")
;;

let calculate_actual input =
  let open TranslationMonad in
  let expr = parse_odefa input in
  let Ast.Expr(clist) = expr in
  let tc = new_translation_context () in
  let new_clist = run tc (condition_clauses clist) in
  show_expr @@ Ast.Expr(new_clist)
;;

_add_test "constrain_arithmetic" @@ fun _ ->
  let input =
    "a = input;
     b = input;
     binop = a + b;"
  in
  let expected = format_expected @@ 
    "a = input;
     b = input;
     m1~0 = a ~ int;
     m2~1 = b ~ int;
     m~2 = m1~0 and m2~1;
     constrain_binop~3 = m~2 ? (binop = a + b) : (ab~4 = abort)"
  in
  let actual = calculate_actual input in
  assert_equal ~printer:Fun.id expected actual
;;


_add_test "constrain_boolean" @@ fun _ ->
  let input =
    "a = true;
     b = false;
     binop = a and b;"
  in
  let expected = format_expected @@
    "a = true;
     b = false;
     m1t~0 = a ~ true;
     m1f~1 = a ~ false;
     m2t~2 = b ~ true;
     m2f~3 = b ~ false;
     m1~4 = m1t~0 or m1f~1;
     m2~5 = m2t~2 or m2f~3;
     m~6 = m1~4 and m2~5;
     constrain_binop~7 = m~6 ? (binop = a and b) : (ab~8 = abort)"
  in
  let actual = calculate_actual input in
  assert_equal ~printer:Fun.id expected actual
;;

_add_test "constrain_application" @@ fun _ ->
  let input =
    "f = fun x -> (ret = x);
     a = input;
     appl = f a;"
  in
  let expected =
    "\n" ^
    "  f = fun x -> (\n" ^
    "  ret = x);\n" ^
    "  a = input;\n" ^
    "  m~0 = f ~ fun;\n" ^
    "  constrain_appl~1 = m~0 ? (appl = f a) : (ab~2 = abort)\n"
  in
  let actual = calculate_actual input in
  assert_equal ~printer:Fun.id expected actual
;;

_add_test "constrain_projection" @@ fun _ ->
  let input =
    "a = input;
     r = {label=a};
     proj = r.label;"
  in
  let expected = format_expected @@
    "a = input;
     r = {label=a};
     m~0 = r ~ {label};
     constrain_proj~1 = m~0 ? (proj = r.label) : (ab~2 = abort)"
  in
  let actual = calculate_actual input in
  assert_equal ~printer:Fun.id expected actual
;;

_add_test "constrain_conditional" @@ fun _ ->
  let input =
    "pred = true;
     cond = pred ? (res1 = true) : (res2 = false);"
  in
  let expected = 
    "\n" ^
    "  pred = true;\n" ^
    "  cond = pred ? (\n" ^
    "                   mt~0 = pred ~ true;\n" ^
    "                   iftrue~2 = mt~0 ? (res1 = true) : (ab~4 = abort)\n" ^
    "                  ) : (\n" ^
    "                         mf~1 = pred ~ false;\n" ^
    "                         iffalse~3 = mf~1 ? (res2 = false) : (ab~5 = abort)\n" ^
    "                        )\n"    
  in
  let actual = calculate_actual input in
  assert_equal ~printer:Fun.id expected actual
;;

(* _add_test "constrain_serial" @@ fun _ -> () *)

(* _add_test "constrain_in_fun" @@ fun_ -> () *)

(* **** For main test module **** *)

let tests = "natodefa translator tests" >::: List.rev !_tests_acc;;
