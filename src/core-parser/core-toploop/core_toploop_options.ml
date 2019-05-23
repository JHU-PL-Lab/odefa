open Batteries;;
open Core_toploop_option_parsers;;

(** This type defines the configuration required by the core toploop to evaluate
    an expression. *)
type configuration =
  { topconf_log_prefix : string
  ; topconf_graph_log_file_name : string
  ; topconf_wddpac_interpreter : bool
  }
;;

(** This function adds to a [BatOptParse.OptParser] a collection of option
    parsers which accept arguments for the core toploop. *)
let add_core_toploop_option_parsers parser=
  (* Add logging options *)
  BatOptParse.OptParser.add parser ~long_name:"log" logging_option;
  (* Adds control over graph log file name. *)
  BatOptParse.OptParser.add parser
    ~long_name:"graph-log-file" graph_log_file_option;

  BatOptParse.OptParser.add parser ~long_name:"wddpac-interpreter"
    ~short_name:'W' wddpac_interpreter_option;
;;

let read_parsed_core_toploop_configuration () =
  { topconf_log_prefix = "_toploop"
  ; topconf_graph_log_file_name =
      Option.get @@ graph_log_file_option.BatOptParse.Opt.option_get ()
  ; topconf_wddpac_interpreter = Option.get @@
      wddpac_interpreter_option.BatOptParse.Opt.option_get ()
  }
;;
