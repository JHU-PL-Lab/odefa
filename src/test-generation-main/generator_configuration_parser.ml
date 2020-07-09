open Batteries;;

open Odefa_ast;;
open Odefa_answer_generation;;

open Ast;;
open Generator_configuration;;

open Odefa_symbolic_interpreter.Interpreter;;

open Cli_parser;;
open Cli_parser_utils;;

type generator_args = {
  ga_generator_configuration : Generator_configuration.configuration;
  ga_filename : string;
  ga_target_point : Ident.t;
  ga_maximum_steps : int option;
  ga_maximum_results : int option;
  ga_exploration_policy : exploration_policy;
  ga_compact_output : bool;
};;

let parse_args () : generator_args =
  let (parsers, cli_parser) = make_cli_parser () in
  (* **** Perform parse **** *)
  try
    let filename = parse_out_filename cli_parser in
    let conf =
          { conf_context_model =
              insist "Context model" parsers.parse_context_stack;
          }
    in
    { ga_generator_configuration = conf;
      ga_filename = filename;
      ga_target_point =
        Ident(insist "Target point" parsers.parse_target_point);
      ga_maximum_steps =
        parsers.parse_max_steps.BatOptParse.Opt.option_get ();
      ga_maximum_results =
        parsers.parse_max_results.BatOptParse.Opt.option_get ();
      ga_exploration_policy =
        insist "Exploration policy" parsers.parse_exploration_policy;
      ga_compact_output =
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
