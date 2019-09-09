(**
   A front-end for the parser library.
*)
open Batteries;;

open Odefa_ast;;
open Odefa_natural;;

open Ast_pp;;
open Translator_options;;

let main () : unit =
  let options = parse_args () in
  let context =
    Translator_utils.new_translation_context
      ~contextual_recursion:(options.ta_contextual_recursion)
      ~suffix:(if options.ta_parseable then "___" else "~")
      ()
  in
  match options.ta_mode with
  | Odefa_natural_to_odefa ->
    let on_expr = On_parse.parse_program IO.stdin in
    on_expr
    |> On_to_odefa.translate ~translation_context:(Some context)
    |> show_expr
    |> print_endline
;;

main ();;
