open Batteries;;
open Jhupllib;;

open Nondeterminism;;
open Pds_reachability_types_stack;;

let logger = Logger_utils.make_logger "Ddpa_pds_dynamic_pop_handler";;
let lazy_logger = Logger_utils.make_lazy_logger "Ddpa_pds_dynamic_pop_handler";;

module Make
    (Store_ops : Ddpa_abstract_stores.Ops.Sig)
    (Struct : (module type of Ddpa_pds_structure_types.Make(Store_ops))
     with module Store_ops = Store_ops)
    (T : (module type of Ddpa_pds_dynamic_pop_types.Make(Store_ops)(Struct))
     with module Store_ops = Store_ops
      and module Struct = Struct)
=
struct
  open Struct;;
  open T;;

  module Stack_element = Pds_continuation;;
  module State = Pds_state;;
  module Targeted_dynamic_pop_action = Pds_targeted_dynamic_pop_action;;
  module Untargeted_dynamic_pop_action = Pds_untargeted_dynamic_pop_action;;
  type stack_action =
    ( Stack_element.t
    , Targeted_dynamic_pop_action.t
    ) pds_stack_action
  ;;
  let perform_targeted_dynamic_pop element action =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () ->
         Printf.sprintf "perform_targeted_dynamic_pop (%s) (%s)"
           (Pds_continuation.show element)
           (show_pds_targeted_dynamic_pop_action action))
      (fun results ->
         String_utils.concat_sep_delim "[" "]" ", "
           (
             results
             |> Enum.clone
             |> Enum.map (String_utils.string_of_list @@
                          show_pds_stack_action Pds_continuation.pp
                            pp_pds_targeted_dynamic_pop_action)
           )
      )
    @@ fun () ->
    Nondeterminism_monad.enum @@
    let open Nondeterminism_monad in
    match action with
    | Discovered_store_2_of_2 ->
      let%orzero Bottom_of_stack = element in
      return []
    | Intermediate_store ->
      let%orzero Continuation_store _ = element in
      return []
  ;;

  let perform_untargeted_dynamic_pop element action =
    Nondeterminism_monad.enum @@
    let open Nondeterminism_monad in
    match action with
    | Do_jump ->
      let%orzero Jump acl1 = element in
      return ([], Program_point_state acl1)
    | Discovered_store_1_of_2 ->
      let%orzero (Continuation_store store) = element in
      return ( [ Pop_dynamic_targeted(Discovered_store_2_of_2) ]
             , Result_state store
             )
  ;;
end;;
