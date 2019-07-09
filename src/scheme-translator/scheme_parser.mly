%{
open Scheme_ast;;
%}

%token <string> IDENTIFIER
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token EOF
%token OPEN_PAREN
%token CLOSE_PAREN

/*
 * Precedences and associativities.  Lower precedences come first.
 */

%start <Scheme_ast.sexpr list> prog

%%

prog:
  | sexpr_list EOF
      { $1 }
  ;

atom:
  | STRING_LITERAL { Atom(AString($1)) }
  | INT_LITERAL { Atom(AInt($1)) }
  ;

group:
  | OPEN_PAREN sexpr_list CLOSE_PAREN { Group($2) }
  ;

sexpr:
  | atom { $1 }
  | group { $1 }
  ;

sexpr_list:
  | sexpr { [$1] }
  | sexpr sexpr_list { $1 :: $2 }
  ;
