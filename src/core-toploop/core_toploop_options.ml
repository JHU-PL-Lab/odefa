open Batteries;;
open Core_toploop_option_parsers;;

(** This type defines the configuration required by the core toploop to evaluate
    an expression. *)
type configuration =
  { topconf_context_stack : (module Ddpa_context_stack.Context_stack) option
  ; topconf_log_prefix : string
  ; topconf_ddpa_log_level : Ddpa_graph_logger.ddpa_graph_logger_level option
  ; topconf_pdr_log_level :
      Pds_reachability_logger_utils.pds_reachability_logger_level option
  ; topconf_analyze_vars : analyze_variables_selection
  ; topconf_disable_evaluation : bool
  ; topconf_disable_inconsistency_check : bool
  ; topconf_disable_analysis : bool
  ; topconf_report_sizes : bool
  }
;;

(** This function adds to a [BatOptParse.OptParser] a collection of option
    parsers which accept arguments for the core toploop. *)
let add_core_toploop_option_parsers parser=
  (* Add logging options *)
  BatOptParse.OptParser.add parser ~long_name:"log" logging_option;
  (* Add ability to select the context stack. *)
  BatOptParse.OptParser.add parser ~long_name:"select-context-stack"
    ~short_name:'S' select_context_stack_option;
  (* Add DDPA graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"ddpa-logging"
    ddpa_logging_option;
  (* Add PDS reachability graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"pdr-logging" pdr_logging_option;
  (* Add control over variables used in toploop analysis. *)
  BatOptParse.OptParser.add parser ~long_name:"analyze-variables"
    analyze_variables_option;
  (* Add control over whether evaluation actually occurs. *)
  BatOptParse.OptParser.add parser ~long_name:"disable-evaluation"
    ~short_name:'E' disable_evaluation_option;
  (* Add control over whether evaluation actually occurs. *)
  BatOptParse.OptParser.add parser ~long_name:"disable-inconsistency-check"
    ~short_name:'I' disable_inconsistency_check_option;
  (* Add control over whether analysis actually occurs. *)
  BatOptParse.OptParser.add parser ~long_name:"disable-analisys"
    ~short_name:'A' disable_analysis_option;
  (* Add ability to report sizes of generated graphs. *)
  BatOptParse.OptParser.add parser ~long_name:"report-sizes"
    report_sizes_option;
;;

let read_parsed_core_toploop_configuration () =
  { topconf_context_stack =
      Option.get @@ select_context_stack_option.BatOptParse.Opt.option_get ()
  ; topconf_log_prefix = "_toploop"
  ; topconf_ddpa_log_level = ddpa_logging_option.BatOptParse.Opt.option_get ()
  ; topconf_pdr_log_level = pdr_logging_option.BatOptParse.Opt.option_get ()
  ; topconf_analyze_vars = Option.get @@
      analyze_variables_option.BatOptParse.Opt.option_get ()
  ; topconf_disable_evaluation = Option.get @@
      disable_evaluation_option.BatOptParse.Opt.option_get ()
  ; topconf_disable_inconsistency_check = Option.get @@
      disable_inconsistency_check_option.BatOptParse.Opt.option_get ()
  ; topconf_disable_analysis = Option.get @@
      disable_analysis_option.BatOptParse.Opt.option_get ()
  ; topconf_report_sizes = Option.get @@
      report_sizes_option.BatOptParse.Opt.option_get ()
  }
;;
