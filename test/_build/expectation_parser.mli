type token =
  | IDENTIFIER of (string)
  | OUTPUT of (string)
  | AT
  | COLON
  | SEMICOLON
  | COMMA
  | EQUAL
  | QUERY
  | ANALYSES
  | RESULTS
  | EVALUATE
  | STUCK
  | WELL_FORMED
  | ILL_FORMED
  | NO_INCONSISTENCIES
  | INCONSISTENCIES_AT
  | OPEN_PAREN
  | CLOSE_PAREN
  | DDPA
  | PLUME
  | SPLUME
  | OSPLUME
  | OPEN_BRACKET
  | CLOSE_BRACKET
  | EOF
  | NATURAL of (int)

val expectation_file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Test_expectation_types.expectation_file
