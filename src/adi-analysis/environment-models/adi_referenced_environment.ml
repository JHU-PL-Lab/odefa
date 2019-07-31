(** Defines an environment model in which environments are referenced (in the
    style of kCFA) when they are established. *)

open Adi_environment_model;;

module Make : Environment_model =
  functor
    (C : Adi_context_model.Context_model)
    (T : Adi_structure_types.Sig with module C = C)
    (M : Adi_monad.Sig with module C = C and module T = T) ->
  struct
    let build_environment _ = M.get_environment ();;
  end;;
