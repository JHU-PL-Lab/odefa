open Batteries;;

module Make
    (Store_ops : Ddpa_abstract_stores.Ops.Sig)
    (Struct : (module type of Ddpa_pds_structure_types.Make(Store_ops))
     with module Store_ops = Store_ops) =
struct
  module Store_ops = Store_ops;;
  module Struct = Struct;;

  type pds_targeted_dynamic_pop_action =
    | Discovered_store_2_of_2
    (* Finishes the Discovered Store rule.  This rule pops the
       bottom-of-stack sentinel to cause a result state to empty the stack.
       Note that the first step of this rule is untargeted. *)
    | Intermediate_store
    (* The Intermediate Store rule.  This rule pops and discards a single store
       from the stack.  It is used to discard stores which were looked up solely
       to ensure that some value exists. *)
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_targeted_dynamic_pop_action =
  struct
    type t = pds_targeted_dynamic_pop_action
    let equal = equal_pds_targeted_dynamic_pop_action
    let compare = compare_pds_targeted_dynamic_pop_action
    let pp = pp_pds_targeted_dynamic_pop_action
    let show = show_pds_targeted_dynamic_pop_action
    let to_yojson = pds_targeted_dynamic_pop_action_to_yojson
  end;;

  type pds_untargeted_dynamic_pop_action =
    | Do_jump
    (** The action for performing basic jump operations. *)
    | Discovered_store_1_of_2
    (* Starts the Discovered Store rule.  Every program point is subject to
       this rule.  This action consumes a store from the stack, forwarding to
       a result state to await the bottom of the stack. *)
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_untargeted_dynamic_pop_action =
  struct
    type t = pds_untargeted_dynamic_pop_action
    let equal = equal_pds_untargeted_dynamic_pop_action
    let compare = compare_pds_untargeted_dynamic_pop_action
    let pp = pp_pds_untargeted_dynamic_pop_action
    let show = show_pds_untargeted_dynamic_pop_action
    let to_yojson = pds_untargeted_dynamic_pop_action_to_yojson
  end;;
end;;
