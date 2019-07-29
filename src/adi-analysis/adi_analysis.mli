(**
   This module serves as the entry point for an implementation of the techniques
   in "Abstracting Definitional Interpreters" (Darais, Labich, Nguyen, Van Horn;
   2017).  As described in "Lightweight Higher-Kinded Polymorphism" (Yallop,
   White; 2014), parameterizing over type constructors in OCaml is quite
   difficult and generally requires sophisticated use of either functors or
   GADTs.  For that reason, we elide the impressive abstractions of the ADI
   paper here and focus on building a single abstract definitional interpreter.

   TODO: describe Make functor and parameters?  Describe analysis features.
 *)

open Adi_types;;

module Make(Spec : Specification) : Analysis;;
