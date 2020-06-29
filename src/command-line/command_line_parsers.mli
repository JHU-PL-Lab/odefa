open Odefa_ddpa;;
open Ddpa_context_stack;;

exception ParseFailure of string;;

(** Verify the existence of a required (positional) argument in the
    command-line input. **)
val insist : string -> 'a BatOptParse.Opt.t -> 'a;;

(** Parser for a argument that can only take a single value.
    Arguments are:
    - invalid_value_err_msg: A custom error message function with arguments
      [arg_name] and [arg].
    - arg_name: The name of the argument.
    - help: The help message to display on --help. 
    - default_option: The default value if a user value isn't given. 
    - parse_fn: The function to parse the arg string to a value. **)
val single_value_parser :
  ?invalid_value_err_msg: (string -> string -> string) ->
  string ->
  string option ->
  'a option ->
  (string -> 'a option) ->
  'a BatOptParse.Opt.t;;

(** Parser for the logging level, e.g. --log=info, --log=debug, or
    --log=trace. **)
val logging_option_parser : unit -> unit BatOptParse.Opt.t;;

(** Parser for the context stack selection, e.g. --context_stack=1ddpa. **)
val select_context_stack_parser :
  unit -> (module Context_stack) BatOptParse.Opt.t;;

(** Extract the filename from the positional arguments of the command-line
    input. **)
val parse_out_filename : BatOptParse.OptParser.t -> string;;