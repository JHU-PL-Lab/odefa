(** Specifies the parametric behavior of an ADI analysis. *)

open Adi_context_model;;
open Adi_environment_model

module type Specification =
sig
  module C : Context_model
  module E : Environment_model
  val name : string
end;;
