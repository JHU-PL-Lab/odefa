open Batteries;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;

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
    | Store_suffix_1_of_2
    (* The first part of the Store Suffix rule.  This step pops the store from
       the stack and holds it, waiting for the store on which to perform the
       operation. *)
    | Store_suffix_2_of_2 of Abstract_store.t
    (* The second part of the Store Suffix rule.  This step pops the suffixing
       operation from the stack and performs it, putting the resulting store
       onto the stack.  The store argument is the store which was popped from
       the stack in the first step. *)
    | Store_parallel_join_1_of_3
    (* The first part of the Store Parallel Join rule, which pops the first
       operand from the stack. *)
    | Store_parallel_join_2_of_3 of Abstract_store.t
    (* The second part of the Store Parallel Join rule, which pops the operator
       from the stack.  The argument is the first operator. *)
    | Store_parallel_join_3_of_3 of Abstract_store.t
    (* The third part of the Store Parallel Join rule.  This part pops the
       second operand, performs the parallel join, and pushes the resulting
       store. *)
    | Store_serial_join_1_of_3
    (* The first part of the Store Serial Join rule, which pops the first
       operand from the stack. *)
    | Store_serial_join_2_of_3 of Abstract_store.t
    (* The second part of the Store Serial Join rule, which pops the operator
       from the stack.  The argument is the first operator. *)
    | Store_serial_join_3_of_3 of Abstract_store.t
    (* The third part of the Store Serial Join rule.  This part pops the
       second operand, performs the Serial join, and pushes the resulting
       store. *)
    | Value_discovery of abstract_var * abstract_value
    (* The Value Discovery rule.  If the top stack element matches the provided
       variable, then we have successfully found it to be assigned to the given
       value. *)
    | Value_alias of abstract_var * abstract_var
    (* The Value Alias rule.  If the top stack element is a lookup for the first
       variable, it should be changed to a lookup for the second variable. *)
    | Stateless_clause_skip_1_of_2 of abstract_var
    (* The first part of the Stateless Clause Skip rule.  If the top stack
       element is a lookup for a variable other than the one captured here, that
       lookup variable is stored and we move to the next step. *)
    | Stateless_clause_skip_2_of_2 of abstract_var
    (* The second part of the Stateless Clause Skip rule.  If the top stack
       element is not a dereference, then it is replaced along with the lookup
       variable.  The variable here is the variable under lookup, not the one to
       be skipped. *)
    | Capture_1_of_3
    (* The first part of the Capture rule.  This pops and keeps the store on the
       top of the stack. *)
    | Capture_2_of_3 of Abstract_store.t
    (* The second part of the Capture rule.  This pops the capture symbol and
       stores the number of elements which need to be popped and stored.  The
       store is the operand popped from the first step. *)
    | Capture_3_of_3 of
        Abstract_store.t *
        Struct.Bounded_capture_size.t *
        Struct.Pds_continuation.t list
    (* The third part of the Capture rule.  This step will pop a stack
       element and then either (1) decrement the bounded capture size and add
       the element to the list or (2) push the store and the captured elements
       back onto the stack to finish the capturing process.  The arguments are
       the store popped in the first step, the number of elements left to
       capture, and the list of elements already popped. *)
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
