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

  type generator;;
  type generation_result;;

  (** Creates a test generator.  Given a configuration, this generator will
      look for paths in the provided expression for reaching the variable with
      the provided identifier.

      If the query is invalid (e.g. the target variable does not appear in the
      expression), an exception is raised from the symbolic intepreter of type
      [Odefa_symbolic_interpreter.Interpreter.Invalid_query]. *)
  val create :
    ?exploration_policy:exploration_policy ->
    configuration -> expr -> ident -> generator;;
  
  (** A convenience routine for running test generation with a generator.  The
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

module Make(Answer : Answer) : Generator;;