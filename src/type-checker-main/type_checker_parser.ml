open Batteries;;

open Odefa_ast;;
open Odefa_ddpa;;
open Odefa_test_generation;;

open Ast;;
open Ddpa_context_stack;;
open Generator_configuration;;

open Odefa_command_line;;
open Command_line_parsers;;

open Odefa_symbolic_interpreter.Interpreter;;

type type_checker_args = {
  tc_filename : string;
  tc_target_var : Ident.t;
  tc_debug : bool;
  tc_generator_configuration : Generator_configuration.configuration;
  tc_maximum_steps : int option;
  tc_maximum_results : int option;
  tc_exploration_policy : exploration_policy;
}
;;

let named_exploration_policies =
  [ (Explore_breadth_first, "bfs");
    (Explore_smallest_relative_stack_length, "relstack-len");
    (Explore_least_relative_stack_repetition, "relstack-rep");
  ]
;;

type parsers =
  { parse_target_var : string BatOptParse.Opt.t;
    parse_debug : bool BatOptParse.Opt.t;
    parse_context_stack : (module Context_stack) BatOptParse.Opt.t;
    parse_max_steps : int BatOptParse.Opt.t;
    parse_max_results : int BatOptParse.Opt.t;
    parse_exploration_policy : exploration_policy BatOptParse.Opt.t;
    parse_logging : unit BatOptParse.Opt.t;
  }
;;

let make_parsers () : parsers =
  { parse_target_var =
      single_value_parser
        "VARIABLE"
        (Some "Specifies the variable to check for type errors from.")
        None
        Option.some;
    parse_debug =
      single_value_parser
        "DEBUG"
        (Some "Specifies whether to accept .odefa files for debugging purposes")
        (Some false)
        (fun x -> try Some(bool_of_string x) with | Failure _ -> None);
    parse_context_stack =
      select_context_stack_parser ();
    parse_max_steps =
      single_value_parser
        "MAX_STEPS"
        (Some ("Specifies the maximum number of steps to take during " ^
               "computation."))
        None
        (fun x -> try Some(int_of_string x) with | Failure _ -> None);
    parse_max_results =
      single_value_parser
        "MAX_RESULTS"
        (Some ("Specifies the maximum number of results to find during " ^
               "computation."))
        None
        (fun x -> try Some(int_of_string x) with | Failure _ -> None);
    parse_exploration_policy =
      begin
        let named_exploration_policies_str =
          begin
            named_exploration_policies
            |> List.map snd
            |> List.map (fun s -> "* " ^ s)
            |> String.concat "\n "
          end
        in
        single_value_parser
          ~invalid_value_err_msg:
            (fun _ str ->
              "Could not understand exploration policy: " ^ str ^ "\n" ^
              "Valid policies are:\n " ^
              named_exploration_policies_str
            )
          "EXPLORATION_POLICY"
          (Some ("Specifies the exploration policy of the evaluation queue."))
          (Some (Explore_breadth_first))
          (fun s ->
            try
              Some(List.assoc_inv s named_exploration_policies)
            with
            | Not_found -> None 
          )
        end;
    parse_logging =
      logging_option_parser ();
  }
;;

let parse_args () : type_checker_args =
  let cli_parser =
    BatOptParse.OptParser.make ~version:Generator_constants.version ()
  in
  let parsers = make_parsers () in
  (* **** Add options **** *)
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'c'
    ~long_name:"context-stack"
    parsers.parse_context_stack;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'t'
    ~long_name:"target-var"
    parsers.parse_target_var;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'d'
    ~long_name:"debug"
    parsers.parse_debug;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'m'
    ~long_name:"maximum-steps"
    parsers.parse_max_steps;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'r'
    ~long_name:"maximum-results"
    parsers.parse_max_results;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'l'
    ~long_name:"log"
    parsers.parse_logging;
  BatOptParse.OptParser.add
    cli_parser
    ~short_name:'e'
    ~long_name:"exploration-policy"
    parsers.parse_exploration_policy;
  (* **** Perform parse **** *)
  try
    let filename = parse_out_filename cli_parser in
    let conf =
          { conf_context_model =
              insist "Context model" parsers.parse_context_stack;
          }
    in
    { tc_generator_configuration = conf;
      tc_filename = filename;
      tc_debug =
        begin
          match parsers.parse_debug.BatOptParse.Opt.option_get () with
          | Some b -> b
          | None -> false
        end;
      tc_target_var =
        Ident(insist "Target point" parsers.parse_target_var);
      tc_maximum_steps =
        parsers.parse_max_steps.BatOptParse.Opt.option_get ();
      tc_maximum_results =
        parsers.parse_max_results.BatOptParse.Opt.option_get ();
      tc_exploration_policy =
        insist "Exploration policy" parsers.parse_exploration_policy;
    }
  with
  | Command_line_parsers.ParseFailure msg ->
    BatOptParse.OptParser.error cli_parser @@ msg;
    raise @@ Jhupllib.Utils.Invariant_failure
      "BatOptParse.OptParser.error was supposed to terminate the program!"
;;