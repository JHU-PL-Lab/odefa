%{
open On_ast;;
module List = BatList;;
exception On_Parse_error of string;;
%}

%token <string> IDENTIFIER
%token <string> VARIANT_LABEL
%token <int> INT_LITERAL
%token <bool> BOOL
%token EOF
%token OPEN_BRACE
%token CLOSE_BRACE
%token COMMA
%token OPEN_PAREN
%token CLOSE_PAREN
%token OPEN_BRACKET
%token CLOSE_BRACKET
%token EQUALS
%token ARROW
%token DOT
%token COLON
%token UNDERSCORE
%token PIPE
%token FUNCTION
%token WITH
%token LET
%token IN
%token REC
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token INT
%token INPUT
%token MATCH
%token PLUS
%token MINUS
%token ASTERISK
%token SLASH
%token PERCENT
%token LESS
%token LESS_EQUAL
%token EQUAL_EQUAL

/*
 * Precedences and associativities.  Lower precedences come first.
 */
%right prec_let                         /* Let ... In ... */
%right prec_fun                         /* function declaration */
%right prec_if                          /* If ... Then ... Else */
%right prec_clause                      /* | x -> ... */
%right prec_match
%left PIPE                              /* | ... */
%right OR                               /* Or */
%right AND                              /* And */
%left EQUAL_EQUAL LESS LESS_EQUAL       /* = < <= */
%right COLON                            /* : */
%left PLUS MINUS ASTERISK SLASH PERCENT /* + - * / % */
%left VARIANT_LABEL                     /* 'A ... */
%left DOT                               /* record access */

%start <On_ast.expr> prog

%%

prog:
  | expr EOF
      { $1 }
  ;

expr:
  | unary_expr
      { $1 }
  | expr PLUS expr
      { Plus($1, $3) }
  | expr MINUS expr
      { Minus($1, $3) }
  | expr ASTERISK expr
      { Times($1, $3) }
  | expr SLASH expr
      { Divide($1, $3) }
  | expr PERCENT expr
      { Modulus($1, $3) }
  | expr LESS expr
      { LessThan($1, $3) }
  | expr LESS_EQUAL expr
      { Leq($1, $3) }
  | expr AND expr
      { And($1, $3) }
  | expr OR expr
      { Or($1, $3) }
  | expr EQUAL_EQUAL expr
      { Equal($1, $3) }
  | FUNCTION param_list ARROW expr %prec prec_fun
      { Function($2, $4) }
  | LET REC fun_sig_list IN expr %prec prec_fun
      { LetRecFun($3, $5) }
  | IF expr THEN expr ELSE expr %prec prec_if
      { If($2, $4, $6) }
  | LET ident_decl EQUALS expr IN expr %prec prec_let
      { Let($2, $4, $6) }
  | LET fun_sig IN expr %prec prec_fun
      { LetFun($2, $4)}
  | expr DOT label
      { RecordProj($1, $3) }
  | INPUT
      { Input }
  | expr COLON expr /* Using ":" for cons to follow Haskell syntax */
      { ListCons($1, $3) }
  | MATCH expr WITH PIPE pattern_list
      { Match($2, $5) }
  | MATCH expr WITH pattern_list
      { Match($2, $4) }
;

fun_sig:
  | ident_decl param_list EQUALS expr
    { Funsig ($1, $2, $4) }

/* Let Rec statements in Odefa-natural are separated by "with".
   ex) let rec foo x y = ...
       with bar a b = ...
       in
*/
fun_sig_list:
  | fun_sig { [$1] }
  | fun_sig WITH fun_sig_list { $1 :: $3 }

unary_expr:
  | NOT simple_expr { Not($2) }
  | VARIANT_LABEL simple_expr { VariantExpr(Variant_label $1, $2) }
  | appl_expr { $1 }

appl_expr:
  | appl_expr simple_expr
    { Appl($1, $2) }
  | simple_expr { $1 }
;

simple_expr:
  | INT_LITERAL
      { Int $1 }
  | BOOL
      { Bool $1 }
  | ident_usage
      { $1 }
  | OPEN_BRACE record_body CLOSE_BRACE
      { Record $2 }
  | OPEN_BRACE CLOSE_BRACE
      { Record (Ident_map.empty) }
  | OPEN_BRACKET list_body CLOSE_BRACKET
      { List $2 }
  | OPEN_BRACKET CLOSE_BRACKET
      { List [] }
  | OPEN_PAREN expr CLOSE_PAREN
      { $2 }
;

/* Records Expressions in Odefa-natural are delimited by commas and
   their values are set using "=".
   ex) {x = 1, y = 2, z = 3} */
record_body:
  | label EQUALS expr
      { let (Label k) = $1 in
        let key = Ident k in
        Ident_map.singleton key $3 }
  | label EQUALS expr COMMA record_body
      { let (Label k) = $1 in
        let key = Ident k in
        let old_map = $5 in
        let dup_check = Ident_map.mem key old_map in
        if dup_check then raise (On_Parse_error "Duplicate label names in record!")
        else
        let new_map = Ident_map.add key $3 old_map in
        new_map
      }
;

param_list:
  | ident_decl param_list { $1 :: $2 }
  | ident_decl { [$1] }
;

label:
  | IDENTIFIER { Label $1 }
;

ident_usage:
  | ident_decl { Var $1 }
;

ident_decl:
  | IDENTIFIER { Ident $1 }
;

/* TODO: Add pattern matching, variants, lists, and list consing */

/* Lists are enclosed in square brackets and delimited by commas
   ex) [1, 2, true]
   Unlike ocaml, natodefa lists may be heterogenous
*/
list_body:
  | expr COMMA list_body { $1 :: $3 }
  | expr { [$1] }
;

pattern_list:
  | pattern_clause PIPE pattern_list
      { $1 :: $3 }
  | pattern_clause %prec prec_match
      { [$1] }
;

pattern_clause:
  | pattern ARROW expr %prec prec_clause
      { ($1, $3) }

pattern:
  | UNDERSCORE { AnyPat }
  | INT { IntPat }
  | BOOL { if $1 then TruePat else FalsePat }
  | OPEN_BRACE rec_pattern_body CLOSE_BRACE { RecPat $2 }
  | VARIANT_LABEL pattern { VariantPat(Variant(Variant_label $1, $2)) }
  | ident_decl { VarPat $1 }
  | FUNCTION { FunPat }
  | OPEN_BRACKET CLOSE_BRACKET { EmptyLstPat }
  | pattern COLON pattern { LstDestructPat($1, $3) }
;

rec_pattern_body:
  | label EQUALS pattern
      { let (Label k) = $1 in
        let key = Ident k in
        Ident_map.singleton key $3 }
  | label EQUALS pattern COMMA rec_pattern_body
      { let (Label k) = $1 in
        let key = Ident k in
        let old_map = $5 in
        let dup_check = Ident_map.mem key old_map in
        if dup_check then raise (On_Parse_error "Duplicate label names in record!")
        else
        let new_map = Ident_map.add key $3 old_map in
        new_map
      }
;