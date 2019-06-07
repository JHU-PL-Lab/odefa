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

open Parsing;;
let _ = parse_error;;
# 2 "expectation_parser.mly"
  open Test_expectation_types;;
# 34 "expectation_parser.ml"
let yytransl_const = [|
  259 (* AT *);
  260 (* COLON *);
  261 (* SEMICOLON *);
  262 (* COMMA *);
  263 (* EQUAL *);
  264 (* QUERY *);
  265 (* ANALYSES *);
  266 (* RESULTS *);
  267 (* EVALUATE *);
  268 (* STUCK *);
  269 (* WELL_FORMED *);
  270 (* ILL_FORMED *);
  271 (* NO_INCONSISTENCIES *);
  272 (* INCONSISTENCIES_AT *);
  273 (* OPEN_PAREN *);
  274 (* CLOSE_PAREN *);
  275 (* DDPA *);
  276 (* PLUME *);
  277 (* SPLUME *);
  278 (* OSPLUME *);
  279 (* OPEN_BRACKET *);
  280 (* CLOSE_BRACKET *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* OUTPUT *);
  281 (* NATURAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\007\000\007\000\
\007\000\007\000\007\000\007\000\006\000\006\000\004\000\004\000\
\005\000\005\000\009\000\010\000\010\000\010\000\011\000\011\000\
\011\000\011\000\014\000\013\000\012\000\015\000\015\000\008\000\
\000\000"

let yylen = "\002\000\
\003\000\002\000\012\000\010\000\003\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\003\000\001\000\003\000\001\000\
\003\000\001\000\006\000\007\000\003\000\001\000\002\000\002\000\
\001\000\001\000\003\000\001\000\001\000\001\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\033\000\000\000\000\000\000\000\000\000\032\000\012\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\001\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
\000\000\015\000\025\000\026\000\000\000\000\000\000\000\000\000\
\000\000\023\000\024\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\004\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\003\000\000\000\000\000\031\000\013\000\000\000\000\000\000\000\
\029\000\019\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\021\000\038\000\061\000\013\000\022\000\
\062\000\023\000\039\000\074\000\033\000\052\000\060\000"

let yysindex = "\005\000\
\253\254\000\000\031\255\000\000\000\000\000\000\000\000\000\000\
\030\255\000\000\007\255\036\000\032\255\027\255\000\000\000\000\
\039\000\000\000\007\255\033\255\035\255\038\255\037\255\000\000\
\000\000\040\255\036\255\046\255\030\255\005\255\044\255\000\000\
\047\255\000\000\000\000\000\000\014\255\048\255\043\255\005\255\
\028\255\000\000\000\000\042\255\005\255\049\255\030\255\052\255\
\000\000\050\255\051\255\034\255\054\255\057\255\030\255\000\000\
\000\000\005\255\056\255\000\000\058\255\059\255\053\255\030\255\
\000\000\005\255\030\255\000\000\000\000\055\255\060\255\062\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\066\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\252\254\063\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\254\254\000\000\000\000\000\000\000\000\000\000\064\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\061\255\000\000\000\000\066\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\252\255\043\000\240\255\008\000\000\000\247\255\
\000\000\009\000\215\255\000\000\000\000\000\000\011\000"

let yytablesize = 85
let yytable = "\016\000\
\022\000\022\000\021\000\021\000\003\000\001\000\017\000\004\000\
\005\000\006\000\007\000\008\000\009\000\022\000\025\000\021\000\
\063\000\004\000\005\000\006\000\007\000\008\000\009\000\046\000\
\063\000\035\000\036\000\015\000\049\000\037\000\015\000\020\000\
\042\000\043\000\014\000\018\000\019\000\051\000\024\000\027\000\
\028\000\026\000\029\000\030\000\031\000\059\000\032\000\040\000\
\045\000\041\000\047\000\048\000\044\000\050\000\059\000\053\000\
\055\000\056\000\057\000\054\000\058\000\064\000\065\000\073\000\
\066\000\006\000\072\000\016\000\018\000\067\000\014\000\034\000\
\071\000\069\000\068\000\070\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000"

let yycheck = "\009\000\
\005\001\006\001\005\001\006\001\008\001\001\000\011\000\011\001\
\012\001\013\001\014\001\015\001\016\001\018\001\019\000\018\001\
\058\000\011\001\012\001\013\001\014\001\015\001\016\001\040\000\
\066\000\021\001\022\001\001\001\045\000\025\001\001\001\005\001\
\019\001\020\001\004\001\000\000\005\001\047\000\000\000\005\001\
\003\001\009\001\006\001\004\001\009\001\055\000\001\001\004\001\
\006\001\003\001\023\001\010\001\005\001\005\001\064\000\004\001\
\006\001\024\001\005\001\010\001\004\001\006\001\005\001\002\001\
\006\001\000\000\007\001\005\001\005\001\017\001\005\001\029\000\
\018\001\066\000\064\000\067\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\024\001"

let yynames_const = "\
  AT\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  EQUAL\000\
  QUERY\000\
  ANALYSES\000\
  RESULTS\000\
  EVALUATE\000\
  STUCK\000\
  WELL_FORMED\000\
  ILL_FORMED\000\
  NO_INCONSISTENCIES\000\
  INCONSISTENCIES_AT\000\
  OPEN_PAREN\000\
  CLOSE_PAREN\000\
  DDPA\000\
  PLUME\000\
  SPLUME\000\
  OSPLUME\000\
  OPEN_BRACKET\000\
  CLOSE_BRACKET\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  OUTPUT\000\
  NATURAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'analysis_expectation) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expectation_list) in
    Obj.repr(
# 22 "expectation_parser.mly"
                                              ( Expectations ((Some _1), _2) )
# 192 "expectation_parser.ml"
               : Test_expectation_types.expectation_file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expectation_list) in
    Obj.repr(
# 23 "expectation_parser.mly"
                         ( Expectations (None, _1) )
# 199 "expectation_parser.ml"
               : Test_expectation_types.expectation_file))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 9 : 'query_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'analysis_list) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'result_list) in
    Obj.repr(
# 29 "expectation_parser.mly"
    ( Analysis_Expectation (_3, _7, _11) )
# 208 "expectation_parser.ml"
               : 'analysis_expectation))
; (fun __caml_parser_env ->
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'analysis_list) in
    Obj.repr(
# 33 "expectation_parser.mly"
    ( Analysis_Expectation ([], _6, []))
# 215 "expectation_parser.ml"
               : 'analysis_expectation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expectation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expectation_list) in
    Obj.repr(
# 35 "expectation_parser.mly"
                                           ( _1::_3 )
# 223 "expectation_parser.ml"
               : 'expectation_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expectation) in
    Obj.repr(
# 36 "expectation_parser.mly"
                ( [_1] )
# 230 "expectation_parser.ml"
               : 'expectation_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "expectation_parser.mly"
             ( Expect_evaluate )
# 236 "expectation_parser.ml"
               : 'expectation))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "expectation_parser.mly"
          ( Expect_stuck )
# 242 "expectation_parser.ml"
               : 'expectation))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "expectation_parser.mly"
                ( Expect_well_formed )
# 248 "expectation_parser.ml"
               : 'expectation))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "expectation_parser.mly"
               ( Expect_ill_formed )
# 254 "expectation_parser.ml"
               : 'expectation))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "expectation_parser.mly"
                       ( Expect_analysis_no_inconsistencies )
# 260 "expectation_parser.ml"
               : 'expectation))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'lookup_var) in
    Obj.repr(
# 44 "expectation_parser.mly"
                                  ( Expect_analysis_inconsistency_at(_2) )
# 267 "expectation_parser.ml"
               : 'expectation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'result) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'result_list) in
    Obj.repr(
# 47 "expectation_parser.mly"
                             ( _1::_3 )
# 275 "expectation_parser.ml"
               : 'result_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'result) in
    Obj.repr(
# 48 "expectation_parser.mly"
           ( [_1] )
# 282 "expectation_parser.ml"
               : 'result_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'query) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'query_list) in
    Obj.repr(
# 51 "expectation_parser.mly"
                           ( _1::_3 )
# 290 "expectation_parser.ml"
               : 'query_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'query) in
    Obj.repr(
# 52 "expectation_parser.mly"
          ( [_1] )
# 297 "expectation_parser.ml"
               : 'query_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'analysis) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'analysis_list) in
    Obj.repr(
# 55 "expectation_parser.mly"
                                 ( _1::_3 )
# 305 "expectation_parser.ml"
               : 'analysis_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'analysis) in
    Obj.repr(
# 56 "expectation_parser.mly"
             ( [_1] )
# 312 "expectation_parser.ml"
               : 'analysis_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'analysis) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'query) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expected_output) in
    Obj.repr(
# 60 "expectation_parser.mly"
    ( Result (_1, _3, _6) )
# 321 "expectation_parser.ml"
               : 'result))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'lookup_var) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'graph_position) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'context) in
    Obj.repr(
# 64 "expectation_parser.mly"
    ( Query(_1, _3, _6) )
# 330 "expectation_parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lookup_var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'graph_position) in
    Obj.repr(
# 65 "expectation_parser.mly"
                                 ( Query(_1, _3, []) )
# 338 "expectation_parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lookup_var) in
    Obj.repr(
# 66 "expectation_parser.mly"
               ( Query(_1, END, []) )
# 345 "expectation_parser.ml"
               : 'query))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 69 "expectation_parser.mly"
                 ( DDPA(_1) )
# 352 "expectation_parser.ml"
               : 'analysis))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 70 "expectation_parser.mly"
                  ( PLUME(_1) )
# 359 "expectation_parser.ml"
               : 'analysis))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "expectation_parser.mly"
           ( SPLUME )
# 365 "expectation_parser.ml"
               : 'analysis))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "expectation_parser.mly"
            ( OSPLUME )
# 371 "expectation_parser.ml"
               : 'analysis))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lookup_var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lookup_var_list) in
    Obj.repr(
# 75 "expectation_parser.mly"
                                     ( _1::_3 )
# 379 "expectation_parser.ml"
               : 'context))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "expectation_parser.mly"
               ( ProgramPoint(_1) )
# 386 "expectation_parser.ml"
               : 'graph_position))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "expectation_parser.mly"
           ( ResultString(_1) )
# 393 "expectation_parser.ml"
               : 'expected_output))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lookup_var) in
    Obj.repr(
# 84 "expectation_parser.mly"
               ( [_1] )
# 400 "expectation_parser.ml"
               : 'lookup_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lookup_var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lookup_var_list) in
    Obj.repr(
# 85 "expectation_parser.mly"
                                     ( _1::_3 )
# 408 "expectation_parser.ml"
               : 'lookup_var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "expectation_parser.mly"
               ( LUVar _1)
# 415 "expectation_parser.ml"
               : 'lookup_var))
(* Entry expectation_file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expectation_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Test_expectation_types.expectation_file)
