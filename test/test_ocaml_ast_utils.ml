open OUnit2;;
open Batteries;;
open Ocaml_ast_utils;;

let show_stringlist l =
  Pp_utils.pp_to_string (Pp_utils.pp_list Format.pp_print_string) l;;

(*VARMATCH TESTING*)

(*varmatch any*)
let varmatch_any _ =
  let
    result = varmatch [%pat? _ ]
  in
  assert_equal [] (String_set.to_list result)
;;

(*varmatch var*)
let varmatch_var _ =
  let
    result = varmatch [%pat? x ]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*varmatch alias*)
let varmatch_alias _ =
  let
    result = varmatch [%pat? Foo (x,y) as foo]
  in
  assert_equal ["foo";"x";"y"] (String_set.to_list result)
;;

(*varmatch constant*)
let varmatch_constant _ =
  let
    result = varmatch [%pat? 4 ]
  in
  assert_equal [] (String_set.to_list result)
;;

(*varmatch interval*)
let varmatch_interval_1 _ =
  let
    result = varmatch [%pat? 'a'..'z']
  in
  assert_equal [] (String_set.to_list result)
;;

let varmatch_interval_2 _ =
  let
    result = varmatch [%pat? 1 .. 6]
  in
  assert_equal [] (String_set.to_list result)
;;

(*varmatch tuple*)
let varmatch_tuple_1 _ =
  let
    result = varmatch [%pat? (x,y)]
  in
  assert_equal ["x";"y"] (String_set.to_list result)
;;

let varmatch_tuple_2 _ =
  let
    result = varmatch [%pat? (4, "z")]
  in
  assert_equal [] (String_set.to_list result)
;;

let varmatch_tuple_3 _ =
  let
    result = varmatch [%pat? ([|a, b, c|], "d", x, Foo y)]
  in
  assert_equal ["a";"b";"c";"x";"y"] (String_set.to_list result)
;;

(*varmatch construct*)
let varmatch_construct_1 _ =
  let
    result = varmatch [%pat? Foo]
  in
  assert_equal [] (String_set.to_list result)
;;

let varmatch_construct_2 _ =
  let
    result = varmatch [%pat? Bar x]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

let varmatch_construct_3 _ =
  let
    result = varmatch [%pat? Baz (x, y, 4, "z")]
  in
  assert_equal ["x";"y"] (String_set.to_list result)
;;

(*varmatch variant*)
let varmatch_variant_1 _ =
  let
    result = varmatch [%pat? `A]
  in
  assert_equal [] (String_set.to_list result)
;;

let varmatch_variant_2 _ =
  let
    result = varmatch [%pat? `A (x,y)]
  in
  assert_equal ["x";"y"] (String_set.to_list result)
;;

(*varmatch record*)
let varmatch_record_1 _ =
  let
    result = varmatch [%pat? {foo = x; bar = y}]
  in
  assert_equal ["x";"y"] (String_set.to_list result)
;;

let varmatch_record_2 _ =
  let
    result = varmatch [%pat? {foo = x; _}]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*varmatch array*)
let varmatch_array_1 _ =
  let
    result = varmatch [%pat? [|x; y; z|]]
  in
  assert_equal ["x";"y";"z"] (String_set.to_list result)
;;

let varmatch_array_2 _ =
  let
    result = varmatch [%pat? [|"x";"y";"z"|]]
  in
  assert_equal [] (String_set.to_list result)

(*varmatch or*)
let varmatch_or_1 _ =
  let
    result = varmatch [%pat? A (x,y) | B (x)]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

let varmatch_or_2 _ =
  let
    result = varmatch [%pat? {foo =  x; bar = y} | {foo = x; bar = z}]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*varmatch constraint*)
let varmatch_constraint _ =
  let
    result = varmatch [%pat? (x : int)]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*varmatch type*)

(*varmatch lazy*)

(*varmatch unpack*)

(*varmatch exception*)
let varmatch_exception _ =
  let
    result = varmatch [%pat? exception Test_ex]
  in
  assert_equal [] (String_set.to_list result)
;;

(*varmatch extension*)
let varmatch_extension _ =
  let
    f () = freevars String_set.empty [%expr [%ext]]
  in
  assert_raises Unflattened_extension f
;;

(*FREEVARS TESTING*)

let freevars_first _ =
  let
    result = freevars String_set.empty [%expr x+y]
  in
  assert_equal ~printer:show_stringlist ["+";"x";"y"] (String_set.to_list result)
;;

(*freevars ident*)
let freevars_ident _ =
  let
    result = freevars String_set.empty [%expr x]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*freevars constant*)
let freevars_constant_1 _ =
  let
    result = freevars String_set.empty [%expr 4]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_constant_2 _ =
  let
    result = freevars String_set.empty [%expr 'a']
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_constant_3 _ =
  let
    result = freevars String_set.empty [%expr false]
  in
  assert_equal [] (String_set.to_list result)
;;

(*freevars let*)
let freevars_let_1 _ =
  let
    result = freevars String_set.empty [%expr let x = 4 in x + 1]
  in
  assert_equal ["+"] (String_set.to_list result)
;;

let freevars_let_2 _ =
  let
    result = freevars String_set.empty [%expr let x = 4 in x + y]
  in
  assert_equal ["+";"y"] (String_set.to_list result)
;;

let freevars_let_3 _ =
  let
    result = freevars String_set.empty
      [%expr let f x = 5 in f 7]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_let_4 _ =
  let
    result = freevars String_set.empty
      [%expr let y = 4 in
             let f x = (x + y) in
             (f 5) + z]
  in
  assert_equal ["+";"z"] (String_set.to_list result)
;;

(*freevars function*)
let freevars_function_1 _ =
  let
    result = freevars String_set.empty [%expr
      function Foo x -> x | Bar (x,y) -> z]
  in
  assert_equal ["z"] (String_set.to_list result)
;;

let freevars_function_2 _ =
  let
    result = freevars String_set.empty
      [%expr function Foo x -> true | _ -> false]
  in
  assert_equal [] (String_set.to_list result)
;;

(*freevars fun*)
let freevars_fun_1 _ =
  let
    result = freevars String_set.empty [%expr
      fun x -> x]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_fun_2 _ =
  let
    result = freevars String_set.empty
      [%expr fun x -> x + y]
  in
  assert_equal ["+";"y"] (String_set.to_list result)
;;

(*freevars apply*)

(*freevars match*)
let freevars_match _ =
  let result = freevars String_set.empty
      [%expr match x with
           | Foo f -> 0
           | _ -> 1]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*freevars try*)

(*freevars tuple*)
let freevars_tuple_1 _ =
  let
    result = freevars String_set.empty [%expr ('a', 45, "string")]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_tuple_2 _ =
  let
    result = freevars String_set.empty [%expr (x, 'a')]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

let freevars_tuple_3 _ =
  let
    result = freevars String_set.empty [%expr let x = 5 in (x, y)]
  in
  assert_equal ["y"] (String_set.to_list result)
;;

(*freevars construct*)
let freevars_construct_1 _ =
  let
    result = freevars String_set.empty [%expr Foo]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_construct_2 _ =
  let
    result = freevars String_set.empty [%expr Foo x]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

let freevars_construct_3 _ =
  let
    result = freevars String_set.empty [%expr Foo (x, "y")]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*freevars variant*)
let freevars_variant_1 _ =
  let
    result = freevars String_set.empty [%expr `A]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_variant_2 _ =
  let
    result = freevars String_set.empty [%expr `A (function x -> x + 1)]
  in
  assert_equal ["+"] (String_set.to_list result)
;;

(*freevars record*)
let freevars_record_1 _ =
  let
    result = freevars String_set.empty
      [%expr {a = 4; b = true}]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_record_2 _ =
  let
    result = freevars String_set.empty
      [%expr {a = (fun x -> x + 1); b = y}]
  in
  assert_equal ["+";"y"] (String_set.to_list result)
;;

(*freevars field*)
let freevars_field _ =
  let
    result = freevars String_set.empty [%expr x.first]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

(*freevars setfield*)
let freevars_setfield_1 _ =
  let
    result = freevars String_set.empty [%expr x.first <- 4]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

let freevars_setfield_2 _ =
  let
    result = freevars String_set.empty [%expr x.first <- y]
  in
  assert_equal ["x";"y"] (String_set.to_list result)
;;

(*freevars array*)
let freevars_array_1 _ =
  let
    result = freevars String_set.empty [%expr [||]]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_array_2 _ =
  let
    result = freevars String_set.empty [%expr [|'a';'b';'c'|]]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_array_3 _ =
  let
    result = freevars String_set.empty [%expr [|(x,y);(a,b)|]]
  in
  assert_equal ["a";"b";"x";"y"] (String_set.to_list result)
;;

(*freevars ifthenelse*)
let freevars_ifthenelse_1 _ =
  let
    result = freevars String_set.empty [%expr if true then 3 else 4]
  in
  assert_equal [] (String_set.to_list result)
;;

let freevars_ifthenelse_2 _ =
  let
    result = freevars String_set.empty [%expr if (f 5) then 1 else 2]
  in
  assert_equal ["f"] (String_set.to_list result)
;;
let freevars_ifthenelse_3 _ =
  let
    result = freevars String_set.empty
      [%expr if (let x = 1 in f x) then y else 0]
  in
  assert_equal ["f";"y"] (String_set.to_list result)
;;

let freevars_ifthenelse_4 _ =
  let
    result = freevars String_set.empty
      [%expr if 3 = 4 then true else false]
  in
  assert_equal ["="] (String_set.to_list result)
;;

(*freevars sequence*)
let freevars_sequence _ =
  let
    result = freevars String_set.empty [%expr x; y]
  in
  assert_equal ["x";"y"] (String_set.to_list result)
;;

(*freevars while*)
let freevars_while _ =
  let
    result = freevars String_set.empty
      [%expr while (f x = false) do (g x) done]
  in
  assert_equal ["=";"f";"g";"x"] (String_set.to_list result)
;;

(*freevars for*)

(*freevars constraint*)
let freevars_constraint_1 _ =
  let
    result = freevars String_set.empty [%expr (x : int)]
  in
  assert_equal ["x"] (String_set.to_list result)
;;

let freevars_constraint_2 _ =
  let
    result = freevars String_set.empty [%expr (f : (int -> bool))]
  in
  assert_equal ["f"] (String_set.to_list result)
;;

(*freevars coerce*)

(*freevars send*)

(*freevars new*)

(*freevars setinstvar*)

(*freevars override*)

(*freevars letmodule*)

(*freevars assert*)
let freevars_assert _ =
  let
    result = freevars String_set.empty [%expr assert x = 1]
  in
  assert_equal ["=";"x"] (String_set.to_list result)
;;

(*freevars lazy*)

(*freevars poly*)

(*freevars object*)

(*freevars newtype*)

(*freevars pack*)

(*freevars open*)

(*freevars extension*)

let tests = "Pds_programming" >::: [

    "varmatch any" >:: varmatch_any;
    "varmatch var" >:: varmatch_var;
    "varmatch alias" >:: varmatch_alias;
    "varmatch constant" >:: varmatch_constant;
    "varmatch interval 1" >:: varmatch_interval_1;
    "varmatch interval 2" >:: varmatch_interval_2;
    "varmatch tuple 1" >:: varmatch_tuple_1;
    "varmatch tuple 2" >:: varmatch_tuple_2;
    "varmatch tuple 3" >:: varmatch_tuple_3;
    "varmatch construct 1" >:: varmatch_construct_1;
    "varmatch construct 2" >:: varmatch_construct_2;
    "varmatch construct 3" >:: varmatch_construct_3;
    "varmatch variant 1" >:: varmatch_variant_1;
    "varmatch variant 2" >:: varmatch_variant_2;
    "varmatch record 1" >:: varmatch_record_1;
    "varmatch record 2" >:: varmatch_record_2;
    "varmatch array 1" >:: varmatch_array_1;
    "varmatch array 2" >:: varmatch_array_2;
    "varmatch or 1" >:: varmatch_or_1;
    "varmatch or 2" >:: varmatch_or_2;
    "varmatch constraint" >:: varmatch_constraint;
    "varmatch exception" >:: varmatch_exception;
    "varmatch extension" >:: varmatch_extension;

    "freevars first" >:: freevars_first;
    "freevars ident" >:: freevars_ident;
    "freevars constant 1" >:: freevars_constant_1;
    "freevars constant 2" >:: freevars_constant_2;
    "freevars constant 3" >:: freevars_constant_3;
    "freevars let 1" >:: freevars_let_1;
    "freevars let 2" >:: freevars_let_2;
    "freevars let 3" >:: freevars_let_3;
    "freevars let 4" >:: freevars_let_4;
    "freevars function 1" >:: freevars_function_1;
    "freevars function 2" >:: freevars_function_2;
    "freevars fun 1" >:: freevars_fun_1;
    "freevars fun 2" >:: freevars_fun_2;
    "freevars match" >:: freevars_match;
    "freevars tuple 1" >:: freevars_tuple_1;
    "freevars tuple 2" >:: freevars_tuple_2;
    "freevars tuple 3" >:: freevars_tuple_3;
    "freevars construct 1" >:: freevars_construct_1;
    "freevars construct 2" >:: freevars_construct_2;
    "freevars construct 3" >:: freevars_construct_3;
    "freevars variant 1" >:: freevars_variant_1;
    "freevars variant 2" >:: freevars_variant_2;
    "freevars record 1" >:: freevars_record_1;
    "freevars record 2" >:: freevars_record_2;
    "freevars field" >:: freevars_field;
    "freevars setfield 1" >:: freevars_setfield_1;
    "freevars setfield 2" >:: freevars_setfield_2;
    "freevars array 1" >:: freevars_array_1;
    "freevars array 2" >:: freevars_array_2;
    "freevars array 3" >:: freevars_array_3;
    "freevars ifthenelse 1" >:: freevars_ifthenelse_1;
    "freevars ifthenelse 2" >:: freevars_ifthenelse_2;
    "freevars ifthenelse 3" >:: freevars_ifthenelse_3;
    "freevars ifthenelse 4" >:: freevars_ifthenelse_4;
    "freevars sequence" >:: freevars_sequence;
    "freevars while" >:: freevars_while;
    "freevars constraint 1" >:: freevars_constraint_1;
    "freevars constraint 2" >:: freevars_constraint_2;
    "freevars assert" >:: freevars_assert
  ]
;;
