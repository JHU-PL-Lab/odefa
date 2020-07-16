open Odefa_ast;;
open Odefa_symbolic_interpreter;;

exception Parse_failure;;

(** The interface of a generic answer, i.e. information that can be extracted
    from a run of the demand-driven symbolic evaluator. *)
module type Answer = sig
  (** The type of the answer *)
  type t;;

  (** A function to extract an answer from the result of a symbolic interpreter
      evaluation, given an expression and a particular stop variable. *)
  val answer_from_result :
    Ast.expr -> Ast.ident -> Interpreter.evaluation_result -> t;;

  (** A function to parse an answer from a string. Mostly used for testing. *)
  val answer_from_string : string -> t;;

  (** Convert the answer into a string. *)
  val show : t -> string;;

  (** Create an empty answer. *)
  val empty : t;;

  (** Predicate on if the answer is empty. *)
  val is_empty : t -> bool;;

  (** Count the number of answers in the data structure. *)
  val count : t -> int;;

  (** Number of (valid) answers in the list. *)
  val count_list : t list -> int;;

  (** True if generating an answer from the result is successful, false
      otherwise. *)
  val generation_successful : t -> bool;;
end;;

(** An input sequence for a single program flow of symbolic evaluation. *)
module Input_sequence : Answer;;

(** The type errors encountered for a single program flow of symbolic
    evaluation. *)
module Type_errors : Answer;;