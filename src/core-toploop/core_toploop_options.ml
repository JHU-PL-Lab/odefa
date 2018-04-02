open Batteries;;
open Core_toploop_option_parsers;;

(** This type defines the configuration required by the core toploop to evaluate
    an expression. *)
type configuration =
  { topconf_max_stack_delta : int option
  ; topconf_log_prefix : string
  ; topconf_ddpa_log_level : Ddpa_analysis_logging.ddpa_logging_level option
  ; topconf_pdr_log_level : Ddpa_analysis_logging.ddpa_logging_level option
  ; topconf_pdr_log_deltas : bool
  ; topconf_graph_log_file_name : string
  ; topconf_analyze_vars : analyze_variables_selection
  ; topconf_disable_evaluation : bool
  ; topconf_disable_inconsistency_check : bool
  ; topconf_disable_analysis : bool
  ; topconf_report_sizes : bool
  ; topconf_wddpac_interpreter : bool
  ; topconf_forward_interpreter : bool
  ; topconf_python_compiler : bool
  ; topconf_call_by_need : bool
  ; topconf_wddpac_interpreter_map : bool
  ; topconf_church_uint : bool
  ; topconf_my_uint : bool
  }
;;

(** This function adds to a [BatOptParse.OptParser] a collection of option
    parsers which accept arguments for the core toploop. *)
let add_core_toploop_option_parsers parser=
  (* Add logging options *)
  BatOptParse.OptParser.add parser ~long_name:"log" logging_option;
  (* Add ability to select the context stack. *)
  BatOptParse.OptParser.add parser ~long_name:"stack-delta-size"
    ~short_name:'S' stack_delta_size_option;
  (* Add DDPA graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"ddpa-logging"
    ddpa_logging_option;
  (* Add PDS reachability graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"pdr-logging" pdr_logging_option;
  (* Add option to log PDS reachability graphs as deltas. *)
  BatOptParse.OptParser.add parser
    ~long_name:"pdr-deltas" pdr_logging_deltas_option;
  (* Adds control over graph log file name. *)
  BatOptParse.OptParser.add parser
    ~long_name:"graph-log-file" graph_log_file_option;
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

  BatOptParse.OptParser.add parser ~long_name:"wddpac-interpreter"
    ~short_name:'W' wddpac_interpreter_option;
  BatOptParse.OptParser.add parser ~long_name:"python-compiler"
    ~short_name:'P' python_compiler_option;
  BatOptParse.OptParser.add parser ~long_name:"forward-interpreter"
    ~short_name:'F' forward_interpreter_option;
  BatOptParse.OptParser.add parser ~long_name:"call-by-need"
    ~short_name:'N' call_by_need_option;
  BatOptParse.OptParser.add parser ~long_name:"wddpac-interpreter-map"
    ~short_name:'M' wddpac_interpreter_map_option;
  BatOptParse.OptParser.add parser ~long_name:"church-uint"
    ~short_name:'C' church_uint_option;
  BatOptParse.OptParser.add parser ~long_name:"my-uint"
    ~short_name:'U' my_uint_option;
;;

let read_parsed_core_toploop_configuration () =
  { topconf_max_stack_delta =
      Option.get @@ stack_delta_size_option.BatOptParse.Opt.option_get ()
  ; topconf_log_prefix = "_toploop"
  ; topconf_ddpa_log_level = ddpa_logging_option.BatOptParse.Opt.option_get ()
  ; topconf_pdr_log_level = pdr_logging_option.BatOptParse.Opt.option_get ()
  ; topconf_pdr_log_deltas =
      (match pdr_logging_deltas_option.BatOptParse.Opt.option_get () with
       | Some b -> b
       | None -> false)
  ; topconf_graph_log_file_name =
      Option.get @@ graph_log_file_option.BatOptParse.Opt.option_get ()
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
  ; topconf_wddpac_interpreter = Option.get @@
      wddpac_interpreter_option.BatOptParse.Opt.option_get ()
  ; topconf_forward_interpreter = Option.get @@
      forward_interpreter_option.BatOptParse.Opt.option_get ()
  ; topconf_python_compiler = Option.get @@
      python_compiler_option.BatOptParse.Opt.option_get ()
  ; topconf_call_by_need  = Option.get @@
      call_by_need_option.BatOptParse.Opt.option_get ()
  ; topconf_wddpac_interpreter_map = Option.get @@
      wddpac_interpreter_map_option.BatOptParse.Opt.option_get ()
  ; topconf_church_uint = Option.get @@
      church_uint_option.BatOptParse.Opt.option_get ()
  ; topconf_my_uint = Option.get @@
      my_uint_option.BatOptParse.Opt.option_get ()
  }
;;
