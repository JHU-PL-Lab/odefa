{
  open Batteries;;

  open Lexing;;
  open Expectation_parser;;
  open Printf;;

  exception LexerError of string;;
}

let digit = ['0'-'9']
let natural = digit+

let ident_start = ['a'-'z' 'A'-'Z' '_' '~']
let ident_cont = ident_start | ['0'-'9']
let ident = ident_start ident_cont*

let output_char = [ ^'`' ]
let output = '`' output_char+ '`'

let whitespace = [' ' '\t']+

rule token = parse
  | whitespace { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | "@" { AT }
  | "&" { AMPERSAND }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "=" { EQUAL }
  | "(" { OPEN_PAREN }
  | ")" { CLOSE_PAREN }
  | "[" { OPEN_BRACKET }
  | "]" { CLOSE_BRACKET }
  | "QUERY" { QUERY }
  | "ANALYSES" { ANALYSES }
  | "RESULTS" { RESULTS }
  | "CONSISTENCIES" { CONSISTENCIES }
  | "EVALUATE" { EVALUATE }
  | "WELL_FORMED" { WELL_FORMED }
  | "ILL_FORMED" { ILL_FORMED }
  | "STUCK" { STUCK }
  | "NO_INCONSISTENCIES" { NO_INCONSISTENCIES }
  | "INCONSISTENCIES_AT" { INCONSISTENCIES_AT }
  | "DDPA" { DDPA }
  | "PLUME" { PLUME }
  | "SPLUME" { SPLUME }
  | "OSKPLUME" { OSKPLUME }
  | "OSMPLUME" { OSMPLUME }
  | natural as n { NATURAL (int_of_string n) }
  | ident as x { IDENTIFIER x }
  | output as str { OUTPUT (String.sub str 1 (String.length str - 2))}
  | eof { EOF }
  | _ as c { raise (LexerError (sprintf "unrecognized character: %c" c)) }
