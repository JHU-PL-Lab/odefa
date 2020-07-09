open Batteries;;

open Odefa_ast;;
open Odefa_answer_generation;;

open Ast;;
open Generator_configuration;;

open Cli_parser;;
open Cli_parser_utils;;

open Odefa_symbolic_interpreter.Interpreter;;

type type_checker_args = {
  tc_filename : string;
  tc_target_var : Ident.t;
  tc_generator_configuration : Generator_configuration.configuration;
  tc_maximum_steps : int option;
  tc_maximum_results : int option;
  tc_exploration_policy : exploration_policy;
  tc_compact_output : bool;
}
;;

let parse_args () : type_checker_args =
  let (parsers, cli_parser) = make_cli_parser Type_checker_version.version_str in
  (* **** Perform parse **** *)
  try
    let filename = parse_out_filename cli_parser in
    let conf =
          { conf_context_model =
              insist "Context model" parsers.parse_context_stack;
          }
    in
    { tc_generator_configuration = conf;
      tc_filename = filename;
      tc_target_var =
        Ident(insist "Target point" parsers.parse_target_point);
      tc_maximum_steps =
        parsers.parse_max_steps.BatOptParse.Opt.option_get ();
      tc_maximum_results =
        parsers.parse_max_results.BatOptParse.Opt.option_get ();
      tc_exploration_policy =
        insist "Exploration policy" parsers.parse_exploration_policy;
      tc_compact_output =
        match (parsers.parse_compact_output.BatOptParse.Opt.option_get ()) with
        | Some b -> b
        | None -> false
    }
  with
  | ParseFailure msg ->
    BatOptParse.OptParser.error cli_parser @@ msg;
    raise @@ Jhupllib.Utils.Invariant_failure
      "BatOptParse.OptParser.error was supposed to terminate the program!"
;;