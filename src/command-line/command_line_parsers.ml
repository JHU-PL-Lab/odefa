open Batteries;;

open Odefa_ddpa;;
open Ddpa_context_stack;;

exception ParseFailure of string;;

(* Throws an exception if a required, non-positional argument is missing. *)
let insist name parser =
  match parser.BatOptParse.Opt.option_get () with
  | None ->
    raise @@ ParseFailure(Printf.sprintf "%s is required." name)
  | Some x -> x
;;

(* Argument that only accepts a single variable *)
let single_value_parser
    ?invalid_value_err_msg:
      (invalid_value_err_msg=fun arg_name arg ->
         Printf.sprintf "Unrecognized %s value: %s" arg_name arg)
    (arg_name: string)
    (help: string option)
    (default_option: 'a option)
    (parse_fn: string -> 'a option)
  : 'a BatOptParse.Opt.t =
  (* Ref to store the argument in *)
  let cell : 'a option ref = ref None in
  (* Read from command line and set argument *)
  let option_set option_name args =
    let fail s = raise @@ BatOptParse.Opt.Option_error(option_name, s) in
    match args with
    | [] ->
      fail @@ Printf.sprintf "Argument required for option %s" option_name
    | [arg] ->
      begin
        match parse_fn arg with
        | None ->
          fail @@ invalid_value_err_msg arg_name arg
        | Some result ->
          begin
            match !cell with
            | None -> cell := Some result (* Success ! *)
            | Some _ ->
              fail @@ Printf.sprintf "Multiple %s values provided" arg_name
          end
      end
    | _ ->
      fail @@ Printf.sprintf "Invalid number of arguments to option %s: %d"
        option_name (List.length args)
  in
  { option_set = option_set;
    option_set_value = (fun value -> cell := Some value);
    option_get = (fun () ->
      if Option.is_some !cell then !cell else default_option);
    option_metavars = [arg_name];
    option_defhelp = help;
  }
;;

(* Even though this argument is a single variable argument, we cannot use
   the single_value_parser function since logging is set as a side effect.
   This is why option_get returns unit. *)
let logging_option_parser () : unit BatOptParse.Opt.t =
  let open Jhupllib.Logger_utils in
  let log_option_set option_name args =
    let fail s = raise @@ BatOptParse.Opt.Option_error(option_name, s) in
    let parse_log_args arguments =
      match arguments with
      | [arg] ->
        begin
          let (module_name_option, module_level) =
            if BatString.exists arg ":" then
              let (module_name, module_level) = String.split ~by:":" arg in
              (Some module_name, module_level)
            else
              (None, arg)
          in
          match level_of_string module_level with
          | Some level -> (module_name_option, level)
          | None -> fail @@ Printf.sprintf "Invalid log level %s" module_level
        end
      | [] ->
        fail @@ Printf.sprintf "Argument required for option %s" option_name
      | _ ->
        fail @@ Printf.sprintf "Invalid number of arguments to option %s: %d"
          option_name (List.length args)
    in
    match parse_log_args args with
    | (Some module_name, level) ->
        set_logging_level_for module_name level
    | (None, level) ->
        set_default_logging_level level
  in
  { option_set = log_option_set;
    option_set_value = (fun _ -> ());
    option_get = (fun () -> Some ());
    option_metavars = ["LOG_INSTR"];
    option_defhelp = Some ("Sets the logging level")
  }
;;

(* Returns the appropriate context stack given the string arg *)
let select_context_stack stack_name =
  if stack_name = "0ddpa" then
    Some (module Ddpa_unit_stack.Stack : Context_stack)
  else if stack_name = "1ddpa" then
    Some (module Ddpa_single_element_stack.Stack : Context_stack)
  else if stack_name = "2ddpa" then
    Some (module Ddpa_two_element_stack.Stack : Context_stack)
  else if String.ends_with stack_name "ddpa" then
    try
      let num_str =
        String.sub stack_name 0 (String.length stack_name - 4)
      in
      let module Stack = Ddpa_n_element_stack.Make(
        struct let size = int_of_string num_str end)
      in
      Some (module Stack : Context_stack)
    with
    | Failure _ -> None
  else
    None
;;

let select_context_stack_parser () =
  single_value_parser
    "CONTEXT_STACK"
    (Some "Specifies the context stack used in CFG construction.")
    (Some (module Ddpa_single_element_stack.Stack : Context_stack))
    (select_context_stack)
;;

let parse_out_filename cli_parser =
  let positional_args = BatOptParse.OptParser.parse_argv cli_parser in
  match positional_args with
  | [] ->
    raise @@ ParseFailure("You must specify a source file.")
  | [filename] ->
    filename
  | _::extras ->
    raise @@ ParseFailure(
      Printf.sprintf "Spurious arguments: %s" (String.join " " extras))
;;