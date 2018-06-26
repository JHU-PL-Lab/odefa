(** This logging option sets the global logging state of the application.  It
    "produces" a unit but, as each argument is parsed, has the side effect of
    configuring the logger. *)
val logging_option : unit BatOptParse.Opt.t

(** This logging option sets the name for the graph log file. *)
val graph_log_file_option : string BatOptParse.Opt.t

val wddpac_interpreter_option : bool BatOptParse.Opt.t
