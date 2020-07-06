(**
   This module defines a process for generating test input sequences.
*)

open Odefa_ast;;

open Ast;;

open Generator_answer;;
open Generator_configuration;;

open Odefa_symbolic_interpreter;;
open Interpreter;;

(** The interface of a generic answer generator. *)
module type Generator = sig
  module Answer : Answer;;

  (** A generator is a tool which attempts to find an answer to lead to a
      particular point in a program. *)
  type generator =
    {
      (** The program being analyzed.  This is stored for reference
          purposes. *)
      gen_program : expr;

      (** The program point we are trying to reach.  This is stored for
          reference purposes. *)
      gen_target : Ident.t;

      (** A function which, given a maximum number of steps to take, attempts
          to reach the point in question.  Here, "steps" are not of any fixed
          amount of effort; however, some maximum step count is required as
          test generation is not decidable.

          If this function is None, then it is known that no additional paths
          will reach the program point in question.  Note that this field is
          not guaranteed to take on the value None regardless of how many
          steps of computation are performed, as test generation is
          undecidable. *)
      generator_fn : (int -> generation_result) option
    }

  and generation_result =
    {
      (** Answers which will lead to the point in question.  These are
          sequences which have been recently discovered.  If empty, then no
          answers were discovered in the provided number of steps. *)
      gen_answers : Answer.t list;

      (** The number of steps which were taken from the called generator to
          reach this result. *)
      gen_steps : int;

      (** An answer generator which will only produce results for paths not
          previously discovered in this result's ancestry.  If it is known that
          no such paths exists, this value is None.  Note that this value may
          be Some even if no such paths exist as test generation is
          undecidable. *)
      gen_generator : generator;
    }
  ;;

  (** Creates a n answer generator.  Given a configuration, this generator will
      look for paths in the provided expression for reaching the variable with
      the provided identifier.

      If the query is invalid (e.g. the target variable does not appear in the
      expression), an exception is raised from the symbolic intepreter of type
      [Odefa_symbolic_interpreter.Interpreter.Invalid_query]. *)
  val create :
    ?exploration_policy:exploration_policy ->
    configuration -> expr -> ident -> generator;;
  
  (** A convenience routine for running generation with a generator.  The
      given optional integer is the maximum number of steps to take.  This
      routine will use the generator to produce results until either results
      have been provably exhausted or the maximum number of steps has been
      reached. If the latter occurs, the returned test_generator will be a Some
      value.  In any case, each result in the provided list is a sequence of
      inputs together with the number of steps required to reach it.

      The generation_callback optional parameter provides results in the form
      of this function's returned values but is called as each new result is
      generated. *)
  val generate_answers :
    ?generation_callback:(Answer.t -> int -> unit) ->
    int option -> generator ->
    (Answer.t list * int) list * generator option;;
end;;

(** A functor to create an answer generator with the Generator interface. *)
module Make(Answer : Answer) : Generator;;