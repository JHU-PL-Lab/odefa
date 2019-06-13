open Batteries;;

open Odefa_abstract_ast;;
open Odefa_ast;;
open Odefa_ddpa;;
open Odefa_interpreter;;
open Odefa_statistics;;
open Odefa_plume;;

open Ast;;
open Ast_pp;;
open Abstract_ast;;
open Ast_wellformedness;;
open Interpreter;;
open Plume_analysis_logging;;
(* open Plume_graph;; *)
open Ddpa_analysis_logging;;
open Ddpa_graph;;
open Source_statistics;;
open Toploop_analysis_wrapper_types;;
open Toploop_option_parsers;;
open Toploop_options;;
open Toploop_types;;
open Toploop_utils;;

exception Invalid_variable_analysis of string;;

type toploop_situation = {
  ts_expr : expr;
  ts_conf : configuration;
  ts_callbacks : callbacks;
}

(* type analysis_report =  *)

let stdout_illformednesses_callback ills =
  print_string "Provided expression is ill-formed:\n";
  List.iter
    (fun ill -> print_string @@ "   " ^ show_illformedness ill ^ "\n")
    ills;
  flush stdout
;;

let stdout_variable_analysis_callback
    var_name site_name context values analysis_name =
  let (LUVar var_str) = var_name in
  print_string ("\n" ^ analysis_name ^ ": ");
  print_string "Lookup of variable ";
  print_string var_str;
  begin
    match site_name with
    | ProgramPoint site_name_str ->
      print_string " from clause ";
      print_string site_name_str;
    | END -> ()
  end;
  begin
    match context with
    | [] -> ()
    | context_list ->
      print_string " in context ";
      let rec loop ss =
        match ss with
        | [] -> print_string "[]"
        | LUVar s::[] -> print_string s
        | LUVar s::ss' ->
          print_string s;
          print_string "|";
          loop ss'
      in
      loop context_list
  end;
  print_endline " yields values:";
  print_string "    ";
  print_string @@ Abs_filtered_value_set.show values;
  print_endline "";
  flush stdout
;;

let stdout_errors_callback errors =
  errors
  |> List.iter
    (fun error ->
       print_string @@ Toploop_analysis_types.show_error error
    );
  flush stdout
;;

let stdout_evaluation_result_callback v env =
  print_endline (show_var v ^ " where " ^ show_evaluation_environment env ^ "\n");
  flush stdout
;;

let stdout_evaluation_failed_callback msg =
  print_endline @@ "Evaluation failed: " ^ msg;
  flush stdout
;;

let stdout_evaluation_disabled_callback () =
  print_endline "Evaluation disabled";
  flush stdout
;;

let stdout_size_report_callback
    ( ddpa_number_of_active_nodes
    , ddpa_number_of_active_non_immediate_nodes
    , ddpa_number_of_edges
    , pds_number_of_nodes
    , pds_number_of_edges
    ) =
  Printf.printf "DDPA number of active nodes (excluding enter and exit nodes that can be inferred): %n.\nDDPA number of active non immediate nodes (excluding enter and exit nodes that can be inferred): %n.\nDDPA number of edges: %n.\nPDS number of nodes: %n.\nPDS number of edges: %n.\n"
    ddpa_number_of_active_nodes
    ddpa_number_of_active_non_immediate_nodes
    ddpa_number_of_edges
    pds_number_of_nodes
    pds_number_of_edges;
  flush stdout
;;

let stdout_source_statistics_callback stats =
  let { ss_num_program_points = num_program_points;
        ss_num_function_definitions = num_function_definitions;
        ss_num_function_calls = num_function_calls;
        ss_num_variable_references = num_variable_references;
        ss_num_non_local_variable_references =
          num_non_local_variable_references;
        ss_num_non_local_variable_references_by_depth =
          num_non_local_variable_references_by_depth;
        ss_max_lexical_depth = max_lexical_depth;
      } = stats in
  Printf.printf "source file program points: %d\nsource file function definitions: %d\nsource file function calls: %d\nsource file variable references: %d\nsource file non-local variable references: %d\nsource file maximum lexical depth: %d\n"
    num_program_points
    num_function_definitions
    num_function_calls
    num_variable_references
    num_non_local_variable_references
    max_lexical_depth;
  Int_map.iter
    (fun depth count ->
       Printf.printf
         "source file non-local variable references at depth %d: %d\n"
         depth count
    )
    num_non_local_variable_references_by_depth
;;

let no_op_callbacks =
  { cb_illformednesses = (fun _ -> ())
  ; cb_variable_analysis = (fun _ _ _ _ _ -> ())
  ; cb_errors = (fun _ -> ())
  ; cb_evaluation_result = (fun _ _ -> ())
  ; cb_evaluation_failed = (fun _ -> ())
  ; cb_evaluation_disabled = (fun _ -> ())
  ; cb_size_report_callback = (fun _ -> ())
  ; cb_source_statistics_callback = (fun _ -> ())
  }
;;

let stdout_callbacks =
  { cb_illformednesses = stdout_illformednesses_callback
  ; cb_variable_analysis = stdout_variable_analysis_callback
  ; cb_errors = stdout_errors_callback
  ; cb_evaluation_result = stdout_evaluation_result_callback
  ; cb_evaluation_failed = stdout_evaluation_failed_callback
  ; cb_evaluation_disabled = stdout_evaluation_disabled_callback
  ; cb_size_report_callback = stdout_size_report_callback
  ; cb_source_statistics_callback = stdout_source_statistics_callback
  }
;;

(* This function takes in a DDPA context model, make a DDPA analysis, and produces
   an analysis wrapper that is more user-friendly.
   We are specifying the return type with ddpa specific logging config because
   this function's return value is used by create_ddpa_logging_config, which
   imposes the constraint that the wrapper must have ddpa logging config.
*)
let ddpaWrapperMaker (context_stack : (module Ddpa_context_stack.Context_stack))
  : (module Analysis_wrapper
      with type logging_config = ddpa_analysis_logging_config) =
  (
    (* We're finally ready to perform some analyses. Unpack the context
       stack. *)
    let module Context_stack = (val context_stack) in
    (* Define the analysis module. *)
    let module Analysis = Ddpa_analysis.Make(Context_stack) in
    (* Define the convenience wrapper. *)
    let module Wrapped_Analysis = Toploop_ddpa_wrapper.Make(Analysis) in
    (module Wrapped_Analysis)
  )
;;

(* TODO: Similar to ddpaWrapperMaker, this function takes in a PLUME context model, make
   a PLUME analysis, and produces an analysis wrapper that's user friendly.
   Probably need to specify the return type with PLUME specific logging config.
*)

let plumeWrapperMaker (context : (module Plume_context_model.Context_model))
  : (module Analysis_wrapper
      with type logging_config = plume_analysis_logging_config) =
  (* We're finally ready to perform some analyses. Unpack the context
     stack. *)
  let module Context_model = (val context) in
  (* Define the analysis module. *)
  let module Analysis = Plume_analysis.Make(Context_model) in
  (* Define the convenience wrapper. *)
  let module Wrapped_Analysis = Toploop_plume_wrapper.Make(Analysis) in
  (module Wrapped_Analysis)
;;

(* This function takes in a situation, which describes the configuration, callbacks,
   and expression of this query.
   We're declaring lconfig here, because we want to give the guarantee that the
   analysis wrapper argument contains a logging config declared, and it's the same
   type as the logging_config parameter's type.

   Creates the analysis, checks for inconsistencies, and performs the analysis.
*)
let analysis_step_general
    (type lconfig)
    (situation : toploop_situation)
    (logging_config : lconfig option)
    (analysis_wrapper : (module Analysis_wrapper
                          with type logging_config = lconfig))
  : analysis_result =
  (* Unpacking the specified situation *)
  let conf = situation.ts_conf in
  let callbacks = situation.ts_callbacks in
  let e = situation.ts_expr in
  (* Create the analysis.  The wrapper performs full closure on it. *)
  let module A = (val analysis_wrapper) in
  let analysis =
    A.create_analysis
      (* The optional param here will have the type of the logging_config arg *)
      ~logging_config: logging_config
      e
  in
  (* We'll now define a couple of functions to perform the
     analysis-related tasks and then call them below. *)
  (* This function performs a simple error check. *)
  let check_for_errors () =
    if conf.topconf_disable_inconsistency_check
    then []
    else
      let module Error_analysis =
        (* TODO: Doesn't work for PLUME *)
        Toploop_analysis.Make(A)
      in
      let errors =
        List.of_enum @@ Error_analysis.find_errors analysis
      in
      callbacks.cb_errors errors;
      errors
  in
  (* This function takes the configuration option describing the variable
     analysis requested on the command line and standardizes the form of
     the request. *)
  let standardize_variable_analysis_request ()
    : (query list) option =
    match conf.topconf_analyze_vars with
    | Analyze_no_variables -> None
    | Analyze_toplevel_variables ->
      Some(
        e
        |> (fun (Expr(cls)) -> cls)
        |> List.enum
        |> Enum.map lift_clause
        |> Enum.map
          (fun (Abs_clause(Abs_var(Ident i), _)) ->
             Query(LUVar(i), END, []))
        |> List.of_enum
      )
    | Analyze_specific_variables lst ->
      (* We got a list of triplets, and we will turn them into Query-s*)
      Some (List.map
              (fun (var, clause, ctx) ->
                 Query(var, clause, ctx))
              lst)
  in
  (* Given a set of variable analysis requests, this function performs
     them. *)
  let analyze_variable_values (requests : query list) : qna list =
    (* We'll need a mapping from variable names to clauses. *)
    let varname_to_clause_map =
      e
      |> Ast_tools.flatten
      |> List.map lift_clause
      |> List.map
        (fun (Abs_clause(Abs_var i,_) as c) -> (i, c))
      |> List.enum
      |> Ident_map.of_enum
    in
    (* This utility function helps us use the mapping. *)
    let lookup_clause_by_ident ident =
      try
        Ident_map.find ident varname_to_clause_map
      with
      | Not_found -> raise @@
        Invalid_variable_analysis(
          Printf.sprintf "No such variable: %s" (show_ident ident))
    in
    (* Perform each of the requested analyses. *)
    requests
    |> List.enum
    |> Enum.map
      (fun try_query ->
         (* Since the queries are wrapped in a constructor, we need to destruct it *)
         let Query(var_name,site_name,context) = try_query in
         let LUVar(var_ident) = var_name in
         let lookup_var = Abs_var (Ident var_ident) in
         let site =
           match site_name with
           | ProgramPoint site_name_str ->
             Unannotated_clause(lookup_clause_by_ident (Ident (site_name_str)))
           | END -> End_clause (lift_var @@ last_var_of e)
         in
         let context_stack =
           match context with
           | [] -> A.C.empty
           | context_vars ->
             context_vars
             |> List.map (fun (LUVar(wrapped_context)) -> wrapped_context)
             |> List.enum
             |> Enum.fold
               (fun a e ->
                  let c = lookup_clause_by_ident (Ident e) in
                  A.C.push c a
               )
               A.C.empty
         in
         let values =
           A.contextual_values_of_variable_from
             lookup_var site context_stack analysis
         in
         let analysis_name = A.name in
         callbacks.cb_variable_analysis
           var_name site_name context values analysis_name;
         QnA(Query(var_name,site_name,context),values)
      )
    |> List.of_enum
  in
  (* At this point, dump the analysis to debugging if appropriate. *)
  lazy_logger `trace
    (* FIXME: Generalize print statement *)
    (fun () -> Printf.sprintf "DDPA analysis: %s"
        (A.show_analysis analysis));
  (* If reporting has been requested, do that too. *)
  if conf.topconf_report_sizes
  then callbacks.cb_size_report_callback @@
    A.get_size analysis;
  (* Now we'll call the above routines. *)
  let errors = check_for_errors () in
  let analyses : qna list =
    match standardize_variable_analysis_request () with
    | None -> []
    | Some requests -> analyze_variable_values requests
  in
  Analysis_result(analyses, errors)
;;

(* Function that creates a logging config specifically for DDPA.

    Parameters
      situation - toploop_situation to get conf, callbacks, and expr

    Return
      ddpa_analysis_logging_config - logging configuration details
      (unit -> unit) - function that allows us to close the log file
*)
let dummy_create_ddpa_logging_config ()
  : ddpa_analysis_logging_config * (unit -> unit) =
  let ddpa_logging_config =
    { ddpa_pdr_logging_level = Log_nothing
    ; ddpa_cfg_logging_level = Log_nothing
    ; ddpa_pdr_deltas = false
    ; ddpa_json_logger = fun _ -> ()
    }
  in
  (* The rest of this is wrapped in a finally so that, if a JSON graph log
     file is created, it will be properly closed even if an exception is
     raised.
  *)
  let close_files = fun () -> ()
  in
  ddpa_logging_config, close_files
;;

(* Function that creates a logging config specifically for Plume.

    Parameters
      situation - toploop_situation to get conf, callbacks, and expr

    Return
      ddpa_analysis_logging_config - logging configuration details
      (unit -> unit) - function that allows us to close the log file
*)
let dummy_create_plume_logging_config ()
  : plume_analysis_logging_config * (unit -> unit) =
  (* let graph_log_file = ref None in *)
  let plume_logging_config =
    { plume_pdr_logging_level = Log_nothing
    ; plume_cfg_logging_level = Log_result
    ; plume_pdr_deltas = false
    ; plume_json_logger =
        fun _ -> ()
        (* (fun json ->
           let file =
             begin
               let file = File.open_out "ddpa_graph.log" in
               graph_log_file := Some file;
               IO.nwrite file "[\n";
               file
             end
           in
           let json_string =
             Yojson.Safe.pretty_to_string ~std:true json
           in
           IO.nwrite file json_string
        ) *)
    }
  in
  (* The rest of this is wrapped in a finally so that, if a JSON graph log
     file is created, it will be properly closed even if an exception is
     raised.
  *)
  (* FIXME: this is terrible :((((( *)
  let close_files = fun () -> ()
    (* (IO.nwrite (Option.get !graph_log_file) "\n]\n" ) ;
    IO.close_out @@ Option.get !graph_log_file *)
  in
  plume_logging_config, close_files
;;

(* NOTE/FIXME: Commented out for easier plume testing *)
(* let create_ddpa_logging_config (situation : toploop_situation)
   : ddpa_analysis_logging_config * (unit -> unit) =
   let conf = situation.ts_conf in
   (* Set up the logging configuration for the analysis. *)
   let ddpa_cfg_logging_level =
    begin
      match conf.topconf_ddpa_log_level with
      | Some level -> level
      | None -> Ddpa_analysis_logging.Log_nothing;
    end
   in
   let ddpa_pdr_logging_level =
    begin
      match conf.topconf_pdr_log_level with
      | Some level -> level
      | None -> Ddpa_analysis_logging.Log_nothing;
    end
   in
   let ddpa_graph_log_file_name = conf.topconf_graph_log_file_name in
   (* Set up the DDPA logging configuration.  This includes the function
     which will write JSON records to a file as they are reported.  For
     cleanup, we keep the file in an option ref.
  *)
   let graph_log_file = ref None in
   let ddpa_logging_config =
    { ddpa_pdr_logging_level = ddpa_pdr_logging_level
    ; ddpa_cfg_logging_level = ddpa_cfg_logging_level
    ; ddpa_pdr_deltas = conf.topconf_pdr_log_deltas
    ; ddpa_json_logger =
        match ddpa_cfg_logging_level, ddpa_pdr_logging_level with
        | Log_nothing, Log_nothing -> (fun _ -> ())
        | _, _ ->
          (fun json ->
             let file =
               begin
                 match !graph_log_file with
                 | None ->
                   let file = File.open_out ddpa_graph_log_file_name in
                   graph_log_file := Some file;
                   IO.nwrite file "[\n";
                   file
                 | Some file ->
                   IO.nwrite file "\n,\n";
                   file
               end
             in
             let json_string =
               Yojson.Safe.pretty_to_string ~std:true json
             in
             IO.nwrite file json_string
          )
    }
   in
   (* The rest of this is wrapped in a finally so that, if a JSON graph log
     file is created, it will be properly closed even if an exception is
     raised.
  *)
   let close_files =
    (fun () ->
       match !graph_log_file with
       | None -> ()
       | Some file ->
         IO.nwrite file "\n]\n";
         IO.close_out file
    )
   in
   ddpa_logging_config, close_files
   ;; *)


(* Function that solely performs analysis (variable analyes and error checking)
   on an expression. General function that is supported by ddpaWrapperMaker,
   create_ddpa_logging_config, analysis_step_general.

   Parameters
    situation - toploop_situation (conf, callbacks, expr)

   Return value
    type analysis_report - dictionary mapping analysis_task to analysis result
*)
let do_analysis_steps (situation : toploop_situation) : analysis_report =
  let conf = situation.ts_conf in
  (* If no one wants an analysis, don't waste the effort. *)
  if conf.topconf_disable_inconsistency_check &&
     conf.topconf_analyze_vars ==
     Analyze_no_variables &&
     not conf.topconf_report_sizes
  then (* Return an empty analysis_report since we didn't do any analyses *)
    Analysis_task_map.empty
  else
    (* We need to build our analysis_report. List.fold_left because we need to
       do this for every analysis_task that we have. *)
    List.fold_left
      (fun analysis_report -> fun atask ->
         match atask with
         | DDPA (_) ->
           (* get information necessary to close the log file *)
           let stack = ddpa_analysis_to_stack atask in
           let ddpaWrapper = ddpaWrapperMaker stack in
           (* NOTE/FIXME: Commented out for easier plume testing *)
           (* let logging_config, finalize = create_ddpa_logging_config situation in *)
           (* NOTE: Currently, this means all of our DDPA analyses log nothing *)
           let logging_config, finalize = dummy_create_ddpa_logging_config () in
           let result =
             (* close the log file regardless of success/failure *)
             ddpaWrapper |> finally finalize
               (* create/perform the analysis here *)
               (analysis_step_general situation
                  (Some logging_config))
           in
           Analysis_task_map.add atask result analysis_report
         (* TODO: fill this out after implementing plume *)
         | PLUME (_) | SPLUME | OSPLUME ->
           let model = plume_analysis_to_stack atask in
           let plumeWrapper = plumeWrapperMaker model in
           let logging_config, finalize = dummy_create_plume_logging_config () in
           let result =
             (* close the log file regardless of success/failure *)
             plumeWrapper |> finally finalize
               (* create/perform the analysis here *)
               (analysis_step_general situation
                  (Some logging_config))
           in
           Analysis_task_map.add atask result analysis_report
      ) Analysis_task_map.empty
      conf.topconf_analyses
;;


(* Function that solely evaluates an expression.

   Parameters
    situation - toploop_situation type to pull out callbacks, conf, e from

   Return
    type evaluation_result describing the result of evaluating the expression.

*)
let do_evaluation situation has_errors =
  let callbacks = situation.ts_callbacks in
  let conf = situation.ts_conf in
  let e = situation.ts_expr in
  match conf.topconf_evaluation_mode with
  | Never_evaluate ->
    begin
      callbacks.cb_evaluation_disabled ();
      Toploop_types.Evaluation_disabled
    end
  | Safely_evaluate when has_errors ->
    Toploop_types.Evaluation_invalidated
  | Safely_evaluate | Always_evaluate ->
    (* try to evaluate the extracted expression *)
    begin
      try
        let v, env = Interpreter.eval e in
        callbacks.cb_evaluation_result v env;
        Toploop_types.Evaluation_completed(v,env)
      with
      | Interpreter.Evaluation_failure s ->
        Toploop_types.Evaluation_failure s
    end
;;


(* Function that evaluates an expression after performing wellformedness checks,
   variable analyses and error checking.

   Paramters
     callbacks - optional callbacks
     conf - configuration
     e - expression

   Return
    type result describing result of toploop processing
*)
let handle_expression
    ?callbacks:(callbacks=no_op_callbacks)
    conf
    e =
  try
    (* Step 1: check for inconsistencies! *)
    check_wellformed_expr e;
    (* Step 2: perform analyses.  This covers both variable analyses and
       error checking. *)
    (* Build situation so that we can pass it around to helper functions *)
    let situation = {ts_expr = e; ts_conf = conf; ts_callbacks = callbacks} in
    let report : analysis_report = do_analysis_steps situation in
    (* Step 3: perform evaluation. *)
    let evaluation_result =
      let error_free =
        Analysis_task_map.values report
        |> Enum.map (fun (Analysis_result(_, errors)) -> List.is_empty errors)
        |> Enum.for_all identity
      in
      do_evaluation situation (not error_free)
    in
    (* Step 4: perform source statistics counting if requested. *)
    if conf.topconf_report_source_statistics
    then callbacks.cb_source_statistics_callback @@
      Source_statistics.calculate_statistics e;
    (* Generate answer. *)
    { illformednesses = []
    ; analysis_report = report
    ; evaluation_result = evaluation_result
    }
  with
  (* wellformedness check didn't pass - format result type to express this *)
  | Illformedness_found(ills) ->
    callbacks.cb_illformednesses ills;
    { illformednesses = ills
    ; analysis_report = Analysis_task_map.empty
    ; evaluation_result = Evaluation_invalidated
    }
;;
