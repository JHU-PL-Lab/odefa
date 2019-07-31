(**
   This module serves as the entry point for an implementation of the techniques
   in "Abstracting Definitional Interpreters" (Darais, Labich, Nguyen, Van Horn;
   2017).  As described in "Lightweight Higher-Kinded Polymorphism" (Yallop,
   White; 2014), parameterizing over type constructors in OCaml is quite
   difficult and generally requires sophisticated use of either functors or
   GADTs.  For that reason, we elide the impressive abstractions of the ADI
   paper here and focus on building a single abstract definitional interpreter.

   The ADI constructed here is context sensitive; the [Make] functor takes a
   [Context_model] as an argument to define context sensitivity.  As in the
   aforecited paper, this ADI has perfect call-return alignment.  Global store
   widening is applied.

   Environment allocation is parametric in this implementation.  A referencing
   environment model produces an ADI which is similar to CFA: precise on
   non-local variables but exponential in the worst case for any top-k-frames
   context sensitivity model.  A copying environment model produces an ADI in
   the style of "Resolving and Exploiting the k-CFA Paradox" (Might,
   Smaragdakis, Van Horn; 2010): polynomial time for top-k-frames but much less
   precise on non-locals.
*)

open Odefa_abstract_ast;;
open Odefa_ast;;

open Abstract_ast;;
open Adi_specification;;
open Ast;;

module type Analysis =
sig
  (** The specification for this analysis. *)
  module S : Specification
  (** A name for this analysis. *)
  val name : string
  (** The type of an analysis structure. *)
  type analysis
  (** Performs an analysis on the provided expression. *)
  val analyze : expr -> analysis
  (** Given an analysis, looks up the values of a particular variable in the
      empty context. *)
  val values_of_variable : abstract_var -> analysis -> Abs_value_set.t
  (** Given an analysis, looks up the values of a particular variable with a
      particular calling stack. *)
  val contextual_values_of_variable :
    abstract_var -> S.C.t -> analysis -> Abs_value_set.t
end;;

module Make(S : Specification) : Analysis with module S = S;;
