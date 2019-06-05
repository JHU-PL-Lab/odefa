{
    open Batteries;;

    open Lexing;;
    open Parser;;
    open Printf;;

    exception LexerError of string;;
}

let digit = ['0'-'9']
let integer = digit+

let ident_start = ['a'-'z' 'A'-'Z' '_']
let ident_cont = ident_start | ['0'-'9']
let ident = ident_start  ident_cont*

let whitespace = [' ' '\t']+

rule token = parse
  | whitespace { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | "@" { AT }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "=" { EQUAL }
  | "(" { OPEN_PAREN }
  | ")" { CLOSE_PAREN }
  | "QUERY" { QUERY }
  | "ANALYSES" { ANALYSES }
  | "RESULTS" { RESULTS }
  | "START" { START }
  | "END" { END }
  | "EVALUATE" { EVALUATE }
  | "WELL-FORMED" { WELL_FORMED }
  | "ILL-FORMED" { ILL_FORMED }
  | "STUCK" { STUCK }
  | "NO-INCONSISTENCIES" { NO_INCONSISTENCIES }
  | "INCONSISTENCIES_AT" { INCONSISTENCIES_AT }
  | ident as x { IDENTIFIER x }
