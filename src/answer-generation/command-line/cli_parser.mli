open Odefa_ddpa;;
open Ddpa_context_stack;;

open Odefa_symbolic_interpreter.Interpreter;;

type parsers =
  { parse_context_stack : (module Context_stack) BatOptParse.Opt.t;
    parse_target_point : string BatOptParse.Opt.t;
    parse_max_steps : int BatOptParse.Opt.t;
    parse_max_results : int BatOptParse.Opt.t;
    parse_exploration_policy : exploration_policy BatOptParse.Opt.t;
    parse_logging : unit BatOptParse.Opt.t;
    parse_compact_output : bool BatOptParse.Opt.t;
  }
;;

val make_cli_parser : unit -> (parsers * BatOptParse.OptParser.t);;