(** Defines an interface for ADI environment models. *)

open Batteries;;

open Odefa_ast;;

open Ast;;

module type Environment_model =
  functor
    (C : Adi_context_model.Context_model)
    (T : Adi_structure_types.Sig with module C = C)
    (M : Adi_monad.Sig with module C = C and module T = T) ->
  sig
    val build_environment : Ident.t Enum.t Lazy.t -> T.environment M.m
  end;;
