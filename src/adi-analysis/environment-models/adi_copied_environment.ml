(** Defines an environment model in which environments are *copied* (in the
    style of mCFA) when they are established. *)

open Batteries;;

open Odefa_ast;;

open Adi_environment_model;;
open Ast;;

module Make : Environment_model =
  functor
    (C : Adi_context_model.Context_model)
    (T : Adi_structure_types.Sig with module C = C)
    (M : Adi_monad.Sig with module C = C and module T = T) ->
  struct
    open M;;

    let build_environment (lazy_env_vars : Ident.t Enum.t Lazy.t)
      : T.environment m =
      let env_vars = Lazy.force lazy_env_vars in
      (* Look up existing bindings *)
      let%bind existing_bindings =
        env_vars
        |> List.of_enum
        |> mapM
          (fun var ->
             let%bind value = lookup var in
             return (var, value)
          )
      in
      let%bind new_environment =
        existing_bindings
        |> List.map
          (fun (var, value) ->
             let%bind address = allocate var in
             let%bind () = store_set address value in
             return (var, address)
          )
        |> sequence
        |> lift List.enum
        |> lift Ident_map.of_enum
      in
      return new_environment
    ;;
  end;;
