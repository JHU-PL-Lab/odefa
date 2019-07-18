open Batteries;;
open Jhupllib;;
open BatOptParse.Opt;;

open Toploop_types;;

open Odefa_ddpa;;

open Logger_utils;;

let logging_option:unit BatOptParse.Opt.t =
  {
    (* Called whenever e.g. "--log debug" appears in the argument list *)
    option_set =
      (fun _ args ->
         let match_string_with_level level_str =
           match level_of_string level_str with
           | Some level -> level
           | None -> failwith ("Invalid log level \"" ^ level_str ^ "\".")
         in
         (match args with
          |[arg] ->
            (let (module_name_option,module_level) =
               if BatString.exists arg "=" then
                 let (module_name,module_level) =
                   String.split ~by:arg "="
                 in (Some module_name,module_level)
               else
                 (None,arg)
             in
             let level' = match_string_with_level module_level in
             match module_name_option with
             |Some(module_name) ->
               set_logging_level_for module_name level'
             |None ->
               set_default_logging_level level'
            )
          | _ -> raise @@ Option_error ("--log","Invalid argument")
         )
      )
    ;
    option_set_value = (fun _ -> ())
    ;
    option_get = (fun () -> Some())
    ;
    option_metavars = ["LOG_INSTR"]
    ;
    option_defhelp = Some("Sets the logging level.")
    ;
  };;

module type Stack = Ddpa_context_stack.Context_stack;;

let select_analysis_option =
  let analysis_module_ref =
    ref ([])
  in
  {
    option_set = (fun option_name args ->
        (
          match args with
          | [] -> (* TODO: Check whether we allow empty string under this option? *)
            raise @@ Option_error (option_name,
                                   Printf.sprintf "You must specify an analysis!")
          | [analyses] ->
            let analysis_list = (BatString.split_on_char ',' analyses) in
            let find_analysis = fun analysis_str ->
              (
                try
                  Toploop_utils.analysis_from_name analysis_str
                with
                | Not_found ->
                  raise @@ Option_error (option_name,
                                         Printf.sprintf "Invalid analysis name: %s" analysis_str)
              )
            in
            if not (List.is_empty (!analysis_module_ref))
            then raise @@ Option_error (option_name,
                                        Printf.sprintf "Duplicate definition of analysis task!")
            else analysis_module_ref := (List.map find_analysis analysis_list)
          | _ ->
            raise @@ Option_error (option_name,
                                   Printf.sprintf "Invalid argument count: %d" (List.length args))
        )
      );
    option_set_value = (fun analysis_module_option ->
        analysis_module_ref := analysis_module_option
      )
    ;
    option_get = (fun () -> Some (!analysis_module_ref))
    ;
    option_metavars = ["ANALYSIS"]
    ;
    option_defhelp = Some "Selects an analysis (0ddpa,1ddpa,2ddpa,ddpaNR,none)."
    ;
  }
;;

type toploop_logging_level =
  | Log_nothing
  | Log_result
  | Log_everything
;;

let toploop_logging_level_option name =
  let logging_level = ref None in
  {
    option_set = (fun option_name args ->
        match args with
        | [level_name] ->
          let level =
            match level_name with
            | "none" -> Log_nothing
            | "result" -> Log_result
            | "all" -> Log_everything
            | _ -> raise @@ Option_error(
                option_name,
                Printf.sprintf "Invalid %s logging level: %s" name level_name)
          in
          logging_level := Some level
        | _ ->
          raise @@ Option_error (
            option_name,
            Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun level -> logging_level := Some level)
    ;
    option_get = (fun () -> !logging_level)
    ;
    option_metavars = ["LOG_LEVEL"]
    ;
    option_defhelp = Some("Selects a " ^ name ^ " logging level (none,result,all).")
    ;
  }
;;

let cfg_logging_option = toploop_logging_level_option "Analysis CFG";;
let pdr_logging_option = toploop_logging_level_option "Analysis PDS";;

let pdr_logging_deltas_option =
  { (BatOptParse.StdOpt.store_true ())
    with option_defhelp =
           Some("Logs reachability graphs as deltas to save space.")
  }
;;

let graph_log_prefix_option =
  { (BatOptParse.StdOpt.str_option
       ~default:"ddpa_graphs" ~metavar:"GRAPH_LOG_FILE" ())
    with option_defhelp =
           Some("Specifies the name for the DDPA graph log file.")
  }
;;

type analyze_variables_selection =
  | Analyze_no_variables
  | Analyze_toplevel_variables
  | Analyze_specific_variables of
      (lookup_var * graph_position * context) list
[@@deriving eq, ord, show]
;;

let analyze_variables_option =
  let variables_to_analyze = ref Analyze_no_variables in
  {
    option_set = (fun option_name args ->
        match args with
        | [analyze_string] ->
          let new_selection =
            match analyze_string with
            | "none" -> Analyze_no_variables
            | "all" -> Analyze_toplevel_variables
            | _ ->
              if String.starts_with analyze_string "only:"
              then
                let components =
                  analyze_string
                  |> String.lchop ~n:5
                  |> (fun x -> String.nsplit x ~by:",")
                in
                let parse_component component =
                  begin
                    match String.nsplit component ~by:"@" with
                    | [name] -> (LUVar name, END, [])
                    | [name;rest] ->
                      begin
                        match String.nsplit rest ~by:":" with
                        | [loc] -> (LUVar name, ProgramPoint loc, [])
                        | [loc;stack] ->
                          begin
                            let stack_parsed = String.nsplit stack ~by:"|" in
                            let stack_elements =
                              List.map (fun var -> LUVar var) stack_parsed
                            in (LUVar name, ProgramPoint loc, stack_elements)
                          end
                        | _ -> raise @@ Option_error (option_name,
                                                      Printf.sprintf "Invalid component string: %s"
                                                        component)
                      end
                    | _ -> raise @@ Option_error (option_name,
                                                  Printf.sprintf "Invalid component string: %s"
                                                    component)
                  end
                in
                Analyze_specific_variables(List.map parse_component components)
              else
                raise @@ Option_error (option_name,
                                       Printf.sprintf "Unrecognized variable analysis mode: %s"
                                         analyze_string)
          in
          variables_to_analyze := new_selection
        | _ ->
          raise @@ Option_error (option_name,
                                 Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun selection -> variables_to_analyze := selection)
    ;
    option_get = (fun () -> Some (!variables_to_analyze))
    ;
    option_metavars = ["ANALYZE_SPEC"]
    ;
    option_defhelp =
      Some("Selects variables to analyze.  Valid options are \"none\" (to \
            perform no variable-specific analysis), \"all\" (to analyze all \
            top-level variables from the end of the program), or \"only\".  \
            If \"only\" is given, it must be followed by a colon and then a \
            comma-separated list of specifications.  A specification is the \
            name of a variable (e.g. \"a\"), optionally followed by \"@\" and \
            a lookup site variable (e.g. \"s\"), optionally followed by \":\" \
            and a pipe-separated list of variable names representing a context \
            stack (from top to bottom).")
    ;
  }
;;

let disable_evaluation_option =
  BatOptParse.StdOpt.store_true ()
;;

let disable_inconsistency_check_option =
  BatOptParse.StdOpt.store_true ()
;;

let disable_analysis_option =
  BatOptParse.StdOpt.store_true ()
;;

let report_sizes_option =
  BatOptParse.StdOpt.store_true ()
;;

let report_source_statistics_option =
  BatOptParse.StdOpt.store_true ()
;;
