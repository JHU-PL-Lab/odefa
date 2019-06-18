%{
  open Test_expectation_types;;
%}

%token <string> IDENTIFIER OUTPUT
%token AT COLON SEMICOLON COMMA EQUAL
%token QUERY ANALYSES RESULTS
%token EVALUATE STUCK WELL_FORMED ILL_FORMED NO_INCONSISTENCIES INCONSISTENCIES_AT CONSISTENCIES
%token OPEN_PAREN CLOSE_PAREN
%token DDPA PLUME SPLUME OSKPLUME OSMPLUME
%token AMPERSAND
%token OPEN_BRACKET CLOSE_BRACKET EOF
%token <int> NATURAL


%type <Test_expectation_types.expectation_file> expectation_file

%start expectation_file

%%

expectation_file:
  | analysis_expectation expectation_list EOF { Expectations ((Some $1), $2) }
  | expectation_list EOF { Expectations (None, $1) }

analysis_expectation:
  | QUERY COLON query_list SEMICOLON
    ANALYSES COLON analysis_list SEMICOLON
    RESULTS COLON result_list SEMICOLON
    CONSISTENCIES COLON analysis_consistency_expectation_list SEMICOLON
    { Analysis_Expectation ($3, $7, $11, $15) }
  | QUERY COLON SEMICOLON
    ANALYSES COLON analysis_list SEMICOLON
    RESULTS COLON SEMICOLON
    CONSISTENCIES COLON analysis_consistency_expectation_list SEMICOLON
    { Analysis_Expectation ([], $6, [], $13)}
  | QUERY COLON query_list SEMICOLON
    ANALYSES COLON analysis_list SEMICOLON
    RESULTS COLON result_list SEMICOLON
    CONSISTENCIES COLON SEMICOLON
    { Analysis_Expectation ($3, $7, $11, [])}
  | QUERY COLON SEMICOLON
    ANALYSES COLON analysis_list SEMICOLON
    RESULTS COLON SEMICOLON
    CONSISTENCIES COLON SEMICOLON
    { Analysis_Expectation ([], $6, [], [])}

expectation_list:
  | expectation SEMICOLON expectation_list { $1::$3 }
  | expectation SEMICOLON { [$1] }

expectation:
  | EVALUATE { Expect_evaluate }
  | STUCK { Expect_stuck }
  | WELL_FORMED { Expect_well_formed }
  | ILL_FORMED { Expect_ill_formed }

analysis_consistency_expectation_list:
  | analysis_consistency_expectation
  AMPERSAND analysis_consistency_expectation_list { $1 :: $3 }
  | analysis_consistency_expectation { [$1] }

inconsistensies_list:
  | inconsistency COMMA inconsistensies_list { $1 :: $3 }
  | inconsistency { [$1] }

result_list:
  | result COMMA result_list { $1::$3 }
  | result { [$1] }

query_list:
  | query COMMA query_list { $1::$3 }
  | query { [$1] }

analysis_list:
  | analysis COMMA analysis_list { $1::$3 }
  | analysis { [$1] }

analysis_consistency_expectation:
  | analysis EQUAL consistency { ($1, $3) }

inconsistency:
  | INCONSISTENCIES_AT lookup_var { Expect_analysis_inconsistency_at($2) }

consistency:
  | NO_INCONSISTENCIES { Expect_analysis_no_inconsistencies }
  | inconsistensies_list { Expect_analysis_inconsistencies $1 }

result:
  | analysis OPEN_PAREN query CLOSE_PAREN EQUAL expected_output
    { Result ($1, $3, $6) }

query:
  | lookup_var AT graph_position AT OPEN_BRACKET context CLOSE_BRACKET
    { Query($1, $3, $6) }
  | lookup_var AT graph_position { Query($1, $3, []) }
  | lookup_var { Query($1, END, []) }

analysis:
  | NATURAL DDPA { DDPA($1) }
  | NATURAL PLUME { PLUME($1) }
  | SPLUME { SPLUME }
  | OSKPLUME { OSKPLUME }
  | OSMPLUME { OSMPLUME }

context:
  | lookup_var COMMA lookup_var_list { $1::$3 }

graph_position:
  | IDENTIFIER { ProgramPoint($1) }

expected_output:
  | OUTPUT { ResultString($1) }

lookup_var_list:
  | lookup_var { [$1] }
  | lookup_var COMMA lookup_var_list { $1::$3 }

lookup_var:
  | IDENTIFIER { LUVar $1}