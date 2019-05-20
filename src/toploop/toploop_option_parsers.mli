open Odefa_ddpa;;

(** This logging option sets the global logging state of the application.  It
    "produces" a unit but, as each argument is parsed, has the side effect of
    configuring the logger. *)
val logging_option : unit BatOptParse.Opt.t

(** This logging option selects a particular DDPA analysis to perform. *)
val select_context_stack_option :
  (module Ddpa_context_stack.Context_stack) option BatOptParse.Opt.t

(** This logging option configures how DDPA DOT graphs are logged. *)
val ddpa_logging_option :
  Ddpa_analysis_logging.ddpa_logging_level BatOptParse.Opt.t

(** This logging option configures how PDS reachability DOT graphs are
    logged. *)
val pdr_logging_option :
  Ddpa_analysis_logging.ddpa_logging_level BatOptParse.Opt.t

(** This logging option controls whether PDRs are logged as deltas. *)
val pdr_logging_deltas_option : bool BatOptParse.Opt.t

(** This logging option sets the name for the graph log file. *)
val graph_log_file_option : string BatOptParse.Opt.t

(** A data type used by the [analyze_variables_option] to express the user's
    selection. **)
type analyze_variables_selection =
  | Analyze_no_variables
    (** Performs no variable analysis. *)
  | Analyze_toplevel_variables
    (** Performs analysis on all top-level variables.  Each such variable is
        examined starting from the end clause and in the empty context. *)
  | Analyze_specific_variables of
      (string * string option * string list option) list
    (** Performs analysis on a specific set of variables.  The first component
        of each triple is the name of the variable.  The second component is the
        unique variable name of the clause at which the analysis starts; if
        absent, it will default to the end clause.  The third component is the
        list of variable names identifying call sites to use in a context stack
        (with the leftmost being the top of the stack).  If absent, the empty
        context stack is used. *)

val equal_analyze_variables_selection :
  analyze_variables_selection -> analyze_variables_selection -> bool
val compare_analyze_variables_selection :
  analyze_variables_selection -> analyze_variables_selection -> int
val pp_analyze_variables_selection :
  Format.formatter -> analyze_variables_selection -> unit
val show_analyze_variables_selection : analyze_variables_selection -> string

(** This option sets a selection for the variables to analyze using DDPA before
    executing the program. *)
val analyze_variables_option :
  analyze_variables_selection BatOptParse.Opt.t

(** This option disables the execution of the provided expressions (and simply
    performs the specified analysis. *)
val disable_evaluation_option : bool BatOptParse.Opt.t

(** This option disables the inconsistency check performed by the analysis. *)
val disable_inconsistency_check_option : bool BatOptParse.Opt.t

(** This option disables the analysis. *)
val disable_analysis_option : bool BatOptParse.Opt.t (* FIXME: this option is pointless, right? *)

(** This option enables reporting of sizes of graphs generated by the analysis. *)
val report_sizes_option : bool BatOptParse.Opt.t

(** This option enables reporting of some simple source file statistics *)
val report_source_statistics_option : bool BatOptParse.Opt.t
