open Batteries;;

open Odefa_ast;;

open Ast;;

type type_checker_args = {
  tc_filename : string;
  tc_target_var : Ident.t;
  tc_debug : bool;
}
;;

(* Argument that only accepts a single variable *)
let single_value_parser
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
    | [str] ->
      begin
        match parse_fn str with
        | None ->
          fail @@ Printf.sprintf "Unrecognized %s value: %s" arg_name str
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

type parsers =
  { parse_target_var : string BatOptParse.Opt.t;
    parse_debug : bool BatOptParse.Opt.t;
  }
;;

let make_parsers () : parsers =
  { parse_target_var =
      single_value_parser
        "VARIABLE"
        (Some "Specifies the variable on which to start typechecking")
        None
        Option.some;
    parse_debug =
      single_value_parser
        "true/false"
        (Some "Specifies whether to typecheck .odefa files for debugging purposes")
        (Some false)
        (fun b -> try Some(bool_of_string b) with | Failure _ -> None);
  }
;;

exception ParseFailure of string;;

let insist name parser =
  match parser.BatOptParse.Opt.option_get () with
  | None ->
    raise @@ ParseFailure(Printf.sprintf "%s is required." name)
  | Some x -> x
;;

let parse_args () : type_checker_args =
  let cli_parser =
    BatOptParse.OptParser.make ~version:Type_checker_version.version_str () in
  let parsers = make_parsers () in
  (* **** Add options **** *)
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'t'
    ~long_name:"target-variable"
    parsers.parse_target_var;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'d'
    ~long_name:"debug"
    parsers.parse_debug;
  (* **** Perform parsing **** *)
  let positional_args = BatOptParse.OptParser.parse_argv cli_parser in
  try
    match positional_args with
    | [] ->
      raise @@ ParseFailure("You must specify a source file.")
    | [filename] ->
      { tc_target_var =
          Ident(insist "Target variable" parsers.parse_target_var);
        tc_debug =
          begin
            match (parsers.parse_debug.BatOptParse.Opt.option_get ()) with
            | Some b -> b
            | None -> false
          end;
        tc_filename = filename;
      }
    | _ :: extras ->
      raise @@ ParseFailure(
          Printf.sprintf "Spurious arguments: %s" (String.join " " extras))
  with
  | ParseFailure msg ->
    BatOptParse.OptParser.error cli_parser @@ msg;
    raise @@ Jhupllib.Utils.Invariant_failure
      "BatOptParse.OptParser.error was supposed to terminate the program!"
;;