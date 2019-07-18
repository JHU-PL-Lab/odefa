open Batteries;;
open Toploop_option_parsers;;
open Toploop_types;;

type evaluation_mode =
  | Never_evaluate
  | Safely_evaluate
  | Always_evaluate
;;

(** This type defines the configuration required by the toploop to evaluate
    an expression. *)
type configuration =
  { topconf_analyses: analysis_task list
  ; topconf_log_prefix : string
  ; topconf_cfg_log_level : toploop_logging_level option
  ; topconf_pdr_log_level : toploop_logging_level option
  ; topconf_pdr_log_deltas : bool
  ; topconf_graph_log_file_name_prefix : string
  ; topconf_analyze_vars : analyze_variables_selection
  ; topconf_evaluation_mode : evaluation_mode
  ; topconf_disable_inconsistency_check : bool
  ; topconf_disable_analysis : bool
  ; topconf_report_sizes : bool
  ; topconf_report_source_statistics : bool
  }
;;

(** This function adds to a [BatOptParse.OptParser] a collection of option
    parsers which accept arguments for the toploop. *)
let add_toploop_option_parsers parser=
  (* Add logging options *)
  BatOptParse.OptParser.add parser ~long_name:"log" logging_option;
  (* Add ability to select analysis *)
  BatOptParse.OptParser.add parser ~long_name:"select-analysis"
    ~short_name:'S' select_analysis_option;
  (* Add cfg logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"analysis-logging"
  cfg_logging_option;
  (* Add PDS reachability graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"pdr-logging" pdr_logging_option;
  (* Add option to log PDS reachability graphs as deltas. *)
  BatOptParse.OptParser.add parser
  ~long_name:"pdr-deltas" pdr_logging_deltas_option;
  (* Adds control over graph log file prefix. *)
  BatOptParse.OptParser.add parser
  ~long_name:"graph-log-prefix" graph_log_prefix_option;
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
  BatOptParse.OptParser.add parser ~long_name:"disable-analysis"
    ~short_name:'A' disable_analysis_option;
  (* Add ability to report sizes of generated graphs. *)
  BatOptParse.OptParser.add parser ~long_name:"report-sizes"
    report_sizes_option;
  (* Add ability to report statistics about analyzed source code. *)
  BatOptParse.OptParser.add parser ~long_name:"report-source-statistics"
    report_source_statistics_option;
;;

let read_parsed_toploop_configuration () =
  {
    (* topconf_context_stack =
       Option.get @@ select_context_stack_option.BatOptParse.Opt.option_get () ; *)
    topconf_analyses = Option.get @@ select_analysis_option.BatOptParse.Opt.option_get ()
  ; topconf_log_prefix = "_toploop"
  ; topconf_cfg_log_level = cfg_logging_option.BatOptParse.Opt.option_get ()
  ; topconf_pdr_log_level = pdr_logging_option.BatOptParse.Opt.option_get ()
  ; topconf_pdr_log_deltas =
      (match pdr_logging_deltas_option.BatOptParse.Opt.option_get () with
       | Some b -> b
       | None -> false)
  ; topconf_graph_log_file_name_prefix =
      Option.get @@ graph_log_prefix_option.BatOptParse.Opt.option_get ()
  ; topconf_analyze_vars = Option.get @@
      analyze_variables_option.BatOptParse.Opt.option_get ()
  ; topconf_evaluation_mode =
      (let disable = Option.get @@
         disable_evaluation_option.BatOptParse.Opt.option_get () in
       if disable then Never_evaluate else
         Safely_evaluate)
  ; topconf_disable_inconsistency_check = Option.get @@
      disable_inconsistency_check_option.BatOptParse.Opt.option_get ()
  ; topconf_disable_analysis = Option.get @@
      disable_analysis_option.BatOptParse.Opt.option_get ()
  ; topconf_report_sizes = Option.get @@
      report_sizes_option.BatOptParse.Opt.option_get ()
  ; topconf_report_source_statistics = Option.get @@
      report_source_statistics_option.BatOptParse.Opt.option_get ()
  }
;;
