(**
   A utility type declaration module.  This is stored separately so that the
   types need not be re-declared on in an interface.
*)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_ddpa;;
open Odefa_interpreter;;
open Odefa_statistics;;

open Ast;;
open Ast_pp;;
open Ddpa_abstract_ast;;
open Interpreter;;
open Source_statistics;;

(* This type describes different families of analyses the users may choose at
   the toploop; this will enable them to run more than one analyses at one
   session.
*)
type analysis_task =
  (* DDPA_context_stack.Context_stack is a type in the module ddpa_context_stack.ml *)
  | DDPA of (module Ddpa_context_stack.Context_stack)
  (* TODO: After PLUME is implemented here, add it in.*)
  (* | Plume of (module Plume_context_model.Context_model) *)
;;

(* This function describes how to tell whether two analysis_task's are equal by
   doing pattern match and then compare the names of the analyses.
*)
let equal_analysis_task (a1 : analysis_task) (a2 : analysis_task) =
  match a1, a2 with
  | DDPA (ctx_stack1), DDPA (ctx_stack2) ->
    let module C1 = (val ctx_stack1) in
    let module C2 = (val ctx_stack2) in
    C1.name = C2.name
    (* TODO: After PLUME is implemented, this function needs to be expanded.*)
;;

(* This function describes how to compare two analysis_task's by
   doing pattern match and then compare the names of the analyses.
*)
let compare_analysis_task (a1 : analysis_task) (a2 : analysis_task) =
  match a1, a2 with
  | DDPA (ctx_stack1), DDPA (ctx_stack2) ->
    let module C1 = (val ctx_stack1) in
    let module C2 = (val ctx_stack2) in
    String.compare C1.name C2.name
    (* TODO: After PLUME is implemented, this function needs to be expanded.*)
;;

(* This function describes how to pretty print analysis task. *)
let pp_analysis_task : analysis_task Pp_utils.pretty_printer =
  fun formatter analysis_task ->
  match analysis_task with
  | DDPA (ctx_stack) ->
    let module C = (val ctx_stack) in
    Format.pp_print_string formatter "DDPA (";
    Format.pp_print_string formatter C.name;
    Format.pp_print_string formatter ")";
    (* TODO: After PLUME is implemented, this function needs to be expanded.*)
  ;;

(* query - stores the variable in question, clause, context *)
type query = Query of string * string option * string list option [@@deriving show];;

(* qna - stores the query and set of filtered values that apply (Big Phi hat) *)
type qna = QnA of query * Abs_filtered_value_set.t [@@deriving show];;

(* analysis_result - stores a list of qna, along with a list of errors *)
type analysis_result = Analysis_result of qna list * Toploop_analysis_types.error list [@@deriving show];;

(* keys for Analysis_task_map with type analysis_task *)
module Analysis_task = struct
  type t = analysis_task;;
  let compare = compare_analysis_task;;
  let pp = pp_analysis_task;;
end;;

(* module necessary for creating analysis_report *)
module Analysis_task_map = struct
  module M = Map.Make(Analysis_task);;
  include M;;
  include Pp_utils.Map_pp(M) (Analysis_task);;
end;;

(* dictionary mapping analysis task to analysis result *)
type analysis_report = analysis_result Analysis_task_map.t [@@deriving show];;

(** Represents the result of evaluating an expression.  This data type also
    captures exceptional cases and toploop configuration properties. *)
type evaluation_result =
  | Evaluation_completed of var * evaluation_environment
  (** The case in which evaluation was successful. *)

  | Evaluation_failure of string
  (** The case in which evaluation became stuck. *)

  | Evaluation_invalidated
  (** The case in which evaluation was not performed due to some kind of
      previous problem (e.g. a well-formedness error). *)

  | Evaluation_disabled
  (** The case in which the user specifically disabled evaluation. *)
[@@deriving show]
;;

(** Represents the information produced by a variable analysis. *)
type variable_analysis =
  (string * string option * string list option) * Abs_filtered_value_set.t
;;
let pp_variable_analysis =
  Pp_utils.pp_tuple
    (Pp_utils.pp_triple
       Format.pp_print_string
       (Pp_utils.pp_option Format.pp_print_string)
       (Pp_utils.pp_option @@ Pp_utils.pp_list Format.pp_print_string))
    Ddpa_abstract_ast.Abs_filtered_value_set.pp
;;

(** Represents the result of processing an expression in the toploop. *)
type result =
  {
    illformednesses : Ast_wellformedness.illformedness list;
    (** A set of ill-formednesses discovered in the expression.  If this set is
        non-empty, then the remaining components of the result will be empty. *)

    analysis_report : analysis_report;
    (** A dictionary between the analysis task and the analysis result *)
    (*TODO: write better descriptions :) *)

    evaluation_result : evaluation_result
    (** The result of evaluating the provided expression.  This will be absent
        if evaluation was disabled, if error checking was enabled and discovered
        errors, or if the expression was ill-formed. *)
  }
[@@deriving show]
;;

(** A record containing the callbacks that the toploop calls during evaluation.
    This allows for a more interactive experience than if the caller waits for
    the entire result to be produced. *)
type callbacks =
  { cb_illformednesses : Ast_wellformedness.illformedness list -> unit
  ; cb_variable_analysis :
      string -> string option -> string list option ->
      Abs_filtered_value_set.t -> unit
  ; cb_errors : Toploop_analysis_types.error list -> unit
  ; cb_evaluation_result : var -> value Environment.t -> unit
  ; cb_evaluation_failed : string -> unit
  ; cb_evaluation_disabled : unit -> unit
  ; cb_size_report_callback : int * int * int * int * int -> unit
  ; cb_source_statistics_callback : source_statistics -> unit
  }
;;
