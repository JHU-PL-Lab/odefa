(**
   A utility type declaration module.  This is stored separately so that the
   types need not be re-declared on in an interface.
*)

open Core_ast;;
open Core_ast_pp;;
(* open Formula;; *)
open Core_interpreter_utils;;
open Core_interpreter;;
(* open Core_interpreter_wddpac_naive_2;; *)

(** Represents the result of evaluating an expression.  This data type also
    captures exceptional cases and toploop configuration properties. *)
type evaluation_result =
  (* | Evaluation_completed of value *)
  | Evaluation_completed of var * evaluation_environment * Formula_type.t * input_mapping
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

(** Represents the result of processing an expression in the toploop. *)
type result =
  {
    illformednesses : Core_ast_wellformedness.illformedness list;
    (** A set of ill-formednesses discovered in the expression.  If this set is
        non-empty, then the remaining components of the result will be empty. *)

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
  { cb_illformednesses : Core_ast_wellformedness.illformedness list -> unit
  ; cb_evaluation_result : var -> value Environment.t -> Formula_type.t -> input_mapping -> unit
  ; cb_evaluation_failed : string -> unit
  ; cb_evaluation_disabled : unit -> unit
  }
;;
