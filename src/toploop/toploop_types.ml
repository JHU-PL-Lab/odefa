(**
   A utility type declaration module.  This is stored separately so that the
   types need not be re-declared on in an interface.
*)

open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Odefa_ast;;
(* open Odefa_ddpa;; *)
open Odefa_interpreter;;
open Odefa_statistics;;

open Ast;;
open Ast_pp;;
open Abstract_ast;;
open Interpreter;;
open Source_statistics;;

(* This type describes different families of analyses the users may choose at
   the toploop; this will enable them to run more than one analyses at one
   session.
*)

type analysis_task =
  | DDPA of int
  | PLUME of int
  | ADI of int
  | SPLUME
  | OSKPLUME
  | OSMPLUME
  | SADI
  [@@deriving show, ord, eq]
;;

type lookup_var = LUVar of string [@@deriving show, eq, ord];;

type graph_position =
  | ProgramPoint of string
  | END
  [@@deriving show, eq, ord]
;;

type context = lookup_var list [@@deriving show, eq, ord];;

type query =
  | Query of lookup_var * graph_position * context [@@deriving show, eq, ord]
;;

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
    Abs_filtered_value_set.pp
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
      lookup_var -> graph_position -> context ->
      Abs_filtered_value_set.t -> string -> unit
  ; cb_errors : Toploop_analysis_types.error list -> unit
  ; cb_evaluation_result : var -> value Environment.t -> unit
  ; cb_evaluation_failed : string -> unit
  ; cb_evaluation_disabled : unit -> unit
  ; cb_size_report_callback : int * int * int * int * int -> unit
  ; cb_analysis_time_report_callback : int -> unit
  ; cb_source_statistics_callback : source_statistics -> unit
  }
;;
