(**
   A front-end for the LittleBang parser library.
*)

open Batteries;;

open Lexing;;

exception Parse_error of exn * int * int * string;;

let handle_parse_error buf f =
  try
    f ()
  with
  | exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error(exn,line,column,tok)
;;

let parse_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    handle_parse_error buf @@ fun () ->
    Swan_generated_parser.delim_expr Swan_generated_lexer.token buf
  in
  LazyList.from_while read_expr;;

let parse_program (input : IO.input) =
  let buf = Lexing.from_input input in
  handle_parse_error buf @@ fun () ->
  Swan_generated_parser.prog Swan_generated_lexer.token buf
;;
