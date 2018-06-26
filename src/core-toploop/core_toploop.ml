open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Core_ast_wellformedness;;
open Core_interpreter;;
open Core_toploop_options;;
open Core_toploop_types;;
open Ddpa_abstract_stores;;

let stdout_illformednesses_callback ills =
  print_string "Provided expression is ill-formed:\n";
  List.iter
    (fun ill -> print_string @@ "   " ^ show_illformedness ill ^ "\n")
    ills;
  flush stdout
;;

let stdout_variable_analysis_callback
    var_name site_name_opt values =
  print_string "Lookup of variable ";
  print_string var_name;
  begin
    match site_name_opt with
    | Some site_name ->
      print_string " from clause ";
      print_string site_name;
    | None -> ()
  end;
  print_endline " yields values:";
  print_string "    ";
  print_string @@ Pp_utils.pp_to_string Abstract_store_set.pp values;
  print_string "\n";
  flush stdout
;;

let stdout_errors_callback errors =
  errors
  |> List.iter
    (fun error ->
       print_string @@ Core_toploop_analysis_types.show_error error
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

let no_op_callbacks =
  { cb_illformednesses = (fun _ -> ())
  ; cb_variable_analysis = (fun _ _ _ -> ())
  ; cb_errors = (fun _ -> ())
  ; cb_evaluation_result = (fun _ _ -> ())
  ; cb_evaluation_failed = (fun _ -> ())
  ; cb_evaluation_disabled = (fun _ -> ())
  ; cb_size_report_callback = (fun _ -> ())
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
  }
;;

let do_evaluation callbacks conf e =
  if conf.topconf_disable_evaluation
  then
    begin
      callbacks.cb_evaluation_disabled ();
      Core_toploop_types.Evaluation_disabled
    end
  else
    begin
      try
        let v:value = Core_interpreter_wddpac_naive.eval e
        in
        (* callbacks.cb_evaluation_result v env; *)
        Core_toploop_types.Evaluation_completed(v)
      with
      | Core_interpreter.Evaluation_failure s ->
        Core_toploop_types.Evaluation_failure s
    end
;;

let handle_expression
    ?callbacks:(callbacks=no_op_callbacks)
    conf
    e =
  try
    (* Step 1: check for inconsistencies! *)
    check_wellformed_expr e;
    (* Step 3: perform evaluation. *)
    let evaluation_result = do_evaluation callbacks conf e
    in
    (* Generate answer. *)
    { illformednesses = []
    ; analyses = [] (* placeholders *)
      ; errors = []
    ; evaluation_result = evaluation_result
    }
  with
  | Illformedness_found(ills) ->
    callbacks.cb_illformednesses ills;
    { illformednesses = ills
    ; analyses = []
    ; errors = []
    ; evaluation_result = Evaluation_invalidated
    }
;;
