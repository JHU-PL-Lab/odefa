open Batteries;;

open Core_ast;;
open Core_ast_pp;;
open Core_ast_wellformedness;;
open Core_interpreter_utils;;
open Core_toploop_types;;
open Core_toploop_options;;
(* open Formula;; *)

let stdout_illformednesses_callback ills =
  print_string "Provided expression is ill-formed:\n";
  List.iter
    (fun ill -> print_string @@ "   " ^ show_illformedness ill ^ "\n")
    ills;
  flush stdout
;;

let stdout_evaluation_result_callback v env formula =
  print_endline (show_var v ^ " where " ^ show_evaluation_environment env ^ " with formula " ^ (string_of_formula formula) ^ "\n");
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

let no_op_callbacks =
  { cb_illformednesses = (fun _ -> ())
  ; cb_evaluation_result = (fun _ _ _ -> ())
  ; cb_evaluation_failed = (fun _ -> ())
  ; cb_evaluation_disabled = (fun _ -> ())
  }
;;

let stdout_callbacks =
  { cb_illformednesses = stdout_illformednesses_callback
  ; cb_evaluation_result = stdout_evaluation_result_callback
  ; cb_evaluation_failed = stdout_evaluation_failed_callback
  ; cb_evaluation_disabled = stdout_evaluation_disabled_callback
  }
;;

let do_evaluation callbacks conf e =
  let v, env, formula =
    if conf.topconf_wddpac_interpreter then
      Core_interpreter_wddpac_naive_2.eval e (* just to prevent conf from being unused *)
    else
      Core_interpreter_wddpac_naive_2.eval e
  in
  begin
    try
      callbacks.cb_evaluation_result v env formula;
      (Core_toploop_types.Evaluation_completed(v,env), formula)
    with
    | Core_interpreter.Evaluation_failure s ->
      (Core_toploop_types.Evaluation_failure s, formula)
  end
;;

let handle_expression
    ?callbacks:(callbacks=no_op_callbacks)
    conf
    e =
  try
    (* Step 1: check for inconsistencies! *)
    check_wellformed_expr e;
    (* Step 2: perform evaluation. *)
    let evaluation_result,_ = do_evaluation callbacks conf e
    (* let evaluation_result = do_evaluation callbacks e *)
    in
    (* Generate answer. *)
    { illformednesses = []
    ; evaluation_result = evaluation_result
    }
  with
  | Illformedness_found(ills) ->
    callbacks.cb_illformednesses ills;
    { illformednesses = ills
    ; evaluation_result = Evaluation_invalidated
    }
;;
