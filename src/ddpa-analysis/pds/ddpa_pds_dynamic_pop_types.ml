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
    | Function_bottom_return_variable_1_of_2 of
        abstract_var * abstract_var * abstract_clause
    (* The first step of the Function Bottom: Return Variable rule.  This step
       is invoked after the Real_flow_huh symbol has been removed from the
       stack.  Its purpose is to pop and keep the store on the stack
       representing the function value found from the Function Bottom: Flow
       Check rule.  The arguments are, in order, the variable we must be looking
       up presently, the variable we will start looking up if the process
       succeeds, and the call site which should be marked when performing
       relative tracing. *)
    | Function_bottom_return_variable_2_of_2 of
        abstract_var * abstract_var * abstract_clause * Abstract_store.t
    (* The second step of the Function Bottom: Return Variable rule.  This step
       verifies that we are looking for the correct variable to trigger entering
       the bottom of the function.  In addition to the arguments for the first
       step, this step also carries the store which contains the function that
       was invoked. *)
    | Function_top_nonlocal_variable of abstract_var * abstract_clause
    (* The Function Top: Non-Local variable rule.  As long as the current lookup
       variable is not equal to the first argument, this action will add stack
       elements suitable for a non-local lookup.  The second argument is the
       call site from which the function was invoked. *)
    | Conditional_top_nonsubject_variable of abstract_var * abstract_var
    (* The Conditional Top: Non-Subject Variable rule.  If the variable
       currently under lookup is neither of the provided variables, then the
       lookup is permitted to proceed through the wiring node. *)
    | Record_projection_stop_1_of_2
    (* The first part of the Record Projection Stop rule.  This step simply
       retrieves and keeps a store from the stack. *)
    | Record_projection_stop_2_of_2 of Abstract_store.t
    (* The second part of the Record Projection Stop rule.  This step takes a
       projection from the stack and performs it on the store given here (which
       was captured during the first step).  It then performs the projection on
       the store's root and adds the new lookup to the stack. *)
    | Filter_immediate_1_of_2
    (* The first part of the filtering rules which can immediately filter their
       stores.  This includes the empty record pattern filtering rules, although
       empty record patterns are not formally immediate.  This part captures
       the store and holds it in wait for the next element of the stack, which
       is assumed to be a pattern instruction. *)
    | Filter_immediate_2_of_2 of Abstract_store.t
    (* The second part of the filtering rules which can immediately filter
       their stores.  This part captures the filter itself and performs the
       appropriate filtering operation, either pushing the store onto the stack
       or otherwise simply giving up (causing the store to be filtered out). *)
    | Filter_nonempty_record_positive_1_of_2 of annotated_clause
    (* The first part of the Filter Nonempty Record Positive rule.  This part
       simply captures and holds a store.  The annotated clause is the location
       of the lookup, which is used to return to this lookup position after
       each label is examined. *)
    | Filter_nonempty_record_positive_2_of_2 of
        annotated_clause * Abstract_store.t
    (* The second part of the Filter Nonempty Record Positive rule.  This part
       retrieves a positive record filter from the stack and applies it as
       appropriate to the store fetched in the first step.  The annotated clause
       is the location of the lookup, which is used to return to this lookup
       position after each label is examined. *)
    | Filter_nonempty_record_negative_1_of_2 of annotated_clause
    (* The first part of the Filter Nonempty Record Negative rules (both of
       them).  This part simply fetches a store from the stack and holds it for
       use in the second part.  The annotated clause is the location of the
       lookup, which is used to return to this point in the event that a record
       label must be refuted. *)
    | Filter_nonempty_record_negative_2_of_2 of
        annotated_clause * Abstract_store.t
    (* The second part of the Filter Nonempty Record Negative rules (both of
       them).  This part processes the negative record pattern on the stack. *)
    | Dereference_stop
    (* The Dereference Stop rule.  This step captures a store from the stack,
       dereferences it, and induces the lookup of the contents. *)
    | Alias_analysis_start of annotated_clause * annotated_clause
    (* The Alias Analysis Start rule.  This step retrieves the current lookup
       variable to set up the alias analysis process.  The arguments are the
       update clause, and the current clause (which is after the update
       clause). *)
    | May_not_alias_1_of_3
    (* The first step of the May Not Alias rule.  This step retrieves the first
       operand of the aliasing operation. *)
    | May_not_alias_2_of_3 of Abstract_store.t
    (* The second step of the May Not Alias rule.  This step retrieves the
       second operand and waits for the lookup variable.  The argument here is
       the first operand. *)
    | May_not_alias_3_of_3 of Abstract_store.t * Abstract_store.t
    (* The third step of the May Not Alias rule.  This step retrieves the lookup
       variable and then performs the May Not Alias analysis. *)
    | May_alias_1_of_3 of abstract_var
    (* The first step of the May Alias rule.  This step retrieves the first
       operand of the aliasing operation.  The argument is the variable being
       assigned to the cell in the update operation. *)
    | May_alias_2_of_3 of abstract_var * Abstract_store.t
    (* The second step of the May Alias rule.  This step retrieves the second
       operand and waits for the lookup variable.  The arguments are the
       variable being assigned to the cell and the first operand. *)
    | May_alias_3_of_3 of abstract_var * Abstract_store.t * Abstract_store.t
    (* The third step of the May Alias rule.  This step retrieves the lookup
       variable and then performs the May Alias analysis.  The arguments are the
       variable being assigned to the cell and the two operands. *)
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
    | Discovered_store_1_of_2
    (* Starts the Discovered Store rule.  Every program point is subject to
       this rule.  This action consumes a store from the stack, forwarding to
       a result state to await the bottom of the stack. *)
    | Do_jump
    (** The action for performing basic jump operations. *)
    | Do_rewind of annotated_clause
    (** The action for performing rewind operations. *)
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
