open Batteries;;
open Core_ast;;
open Core_ast_pp;;
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
    | Store_suffix_2_of_2 of Abstract_store_witness_registry.escorted_witness
    (* The second part of the Store Suffix rule.  This step pops the suffixing
       operation from the stack and performs it, putting the resulting store
       onto the stack.  The store argument is the store which was popped from
       the stack in the first step. *)
    | Store_parallel_join_1_of_3
    (* The first part of the Store Parallel Join rule, which pops the first
       operand from the stack. *)
    | Store_parallel_join_2_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The second part of the Store Parallel Join rule, which pops the operator
       from the stack.  The argument is the first operator. *)
    | Store_parallel_join_3_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The third part of the Store Parallel Join rule.  This part pops the
       second operand, performs the parallel join, and pushes the resulting
       store. *)
    | Store_serial_join_1_of_3
    (* The first part of the Store Serial Join rule, which pops the first
       operand from the stack. *)
    | Store_serial_join_2_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The second part of the Store Serial Join rule, which pops the operator
       from the stack.  The argument is the first operator. *)
    | Store_serial_join_3_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The third part of the Store Serial Join rule.  This part pops the
       second operand, performs the serial join, and pushes the resulting
       store. *)
    | Store_bind_1_of_2
    (* The first part of the Store Bind rule, which pops the first
       operand from the stack. *)
    | Store_bind_2_of_2 of
        Abstract_store_witness_registry.escorted_witness
    (* The second part of the Store Bind rule.  This part performs the bind, and
       pushes the resulting store. *)
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
    | Capture_2_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The second part of the Capture rule.  This pops the capture symbol and
       stores the number of elements which need to be popped and stored.  The
       store is the operand popped from the first step. *)
    | Capture_3_of_3 of
        Abstract_store_witness_registry.escorted_witness *
        Struct.Bounded_capture_size.t *
        Struct.Pds_continuation.t list
    (* The third part of the Capture rule.  This step will pop a stack
       element and then either (1) decrement the bounded capture size and add
       the element to the list or (2) push the store and the captured elements
       back onto the stack to finish the capturing process.  The arguments are
       the store popped in the first step, the number of elements left to
       capture, and the list of elements already popped. *)
    | Function_bottom_return_variable of
        abstract_var * abstract_var * abstract_clause
    (* The Function Bottom: Return Variable rule.  This action is invoked after
       the Real_flow_huh symbol has been removed from the stack.  This step pops
       the store and verifies that we are looking for the correct variable to
       enter the function. *)
    | Function_top_nonlocal_variable of abstract_var * abstract_clause * annotated_clause
    (* The Function Top: Non-Local variable rule.  As long as the current lookup
       variable is not equal to the first argument, this action will add stack
       elements suitable for a non-local lookup.  The second argument is the
       call site from which the function was invoked. *)
    | Conditional_top_nonsubject_variable_positive of
        abstract_var * abstract_var * annotated_clause * pattern
    (* The Conditional Top: Non-Subject Variable Positive rule.  If the variable
       currently under lookup is neither of the provided variables, then the
       lookup is permitted to proceed through the wiring node. *)
    | Conditional_top_nonsubject_variable_negative of
        abstract_var * abstract_var * annotated_clause * pattern
    (* The Conditional Top: Non-Subject Variable Negative rule.  If the variable
       currently under lookup is neither of the provided variables, then the
       lookup is permitted to proceed through the wiring node. *)
    | Record_projection_stop_1_of_2
    (* The first part of the Record Projection Stop rule.  This step simply
       retrieves and keeps a store from the stack. *)
    | Record_projection_stop_2_of_2 of
        Abstract_store_witness_registry.escorted_witness
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
    | Filter_immediate_2_of_2 of
        Abstract_store_witness_registry.escorted_witness
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
        annotated_clause * Abstract_store_witness_registry.escorted_witness
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
        annotated_clause * Abstract_store_witness_registry.escorted_witness
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
    | May_not_alias_2_of_3 of Abstract_store_witness_registry.escorted_witness
    (* The second step of the May Not Alias rule.  This step retrieves the
       second operand and waits for the lookup variable.  The argument here is
       the first operand. *)
    | May_not_alias_3_of_3 of
        Abstract_store_witness_registry.escorted_witness *
        Abstract_store_witness_registry.escorted_witness
    (* The third step of the May Not Alias rule.  This step retrieves the lookup
       variable and then performs the May Not Alias analysis. *)
    | May_alias_1_of_3 of abstract_var
    (* The first step of the May Alias rule.  This step retrieves the first
       operand of the aliasing operation.  The argument is the variable being
       assigned to the cell in the update operation. *)
    | May_alias_2_of_3 of
        abstract_var * Abstract_store_witness_registry.escorted_witness
    (* The second step of the May Alias rule.  This step retrieves the second
       operand and waits for the lookup variable.  The arguments are the
       variable being assigned to the cell and the first operand. *)
    | May_alias_3_of_3 of
        abstract_var *
        Abstract_store_witness_registry.escorted_witness *
        Abstract_store_witness_registry.escorted_witness
    (* The third step of the May Alias rule.  This step retrieves the lookup
       variable and then performs the May Alias analysis.  The arguments are the
       variable being assigned to the cell and the two operands. *)
    | Stateful_immediate_clause_skip of abstract_var
    (* The Stateful Immediate Clause Skip rule.  This step verifies that we are
       not looking for the variable in the argument.  The clause has already
       been verified to be immediate and non-stateful, so our stateful lookup
       is permitted to ignore it. *)
    | Side_effect_search_start_function_flow_check of
        annotated_clause * annotated_clause
    (* The Side Effect Search Start: Function Flow Check rule.  The arguments
       are the wiring node at which the search is starting and the node
       following that wiring node. *)
    | Side_effect_search_start_function_flow_validated_1_of_2 of
        annotated_clause * annotated_clause
    (* The first step of the Side Effect Search Start: Function Flow Validated
       rule.  The arguments are the wiring node at which the search is starting
       and the node following that wiring node.  This step pops and keeps a
       store and then waits for the lookup variable. *)
    | Side_effect_search_start_function_flow_validated_2_of_2 of
        annotated_clause * annotated_clause *
        Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search Start: Function Flow Validated
       rule. The arguments are the wiring node at which the search is starting,
       the node following that wiring node, and the store which was popped in
       the first step.  This step initiates the side effect search. *)
    | Side_effect_search_start_conditional_positive of
        annotated_clause * annotated_clause
    (* The Side Effect Search Start: Conditional Positive rule.  The arguments
       for this step are the wiring clause that triggered the start of the
       search and the clause immediately following that wiring clause. *)
    | Side_effect_search_start_conditional_negative of
        annotated_clause * annotated_clause
    (* The Side Effect Search Start: Conditional Negative rule.  The arguments
       for this step are the wiring clause that triggered the start of the
       search and the clause immediately following that wiring clause. *)
    | Side_effect_search_immediate_clause_skip
    (* The Side Effect Search Immediate Clause Skip rule.  During a side effect
       search, the only meaningful immediate clause is the cell update; no other
       clauses have an effect on lookup.  This action verifies that a side
       effect search is under way by peeking at the top stack element. *)
    | Side_effect_search_function_bottom_flow_check of
        annotated_clause * abstract_clause
    (* The Side Effect Search: Function Bottom: Flow Check rule.  The arguments
       are the clause from which the check is being performed and the function
       call site. *)
    | Side_effect_search_function_bottom_return_variable_1_of_2 of
        annotated_clause
    (* The first step of the Side Effect Search: Function Bottom: Return
       Variable rule.  This step retrieves a store from the stack in wait for
       the side effect lookup variable.  The argument to this step is the wiring
       node where we are checking function flow. *)
    | Side_effect_search_function_bottom_return_variable_2_of_2 of
        annotated_clause * Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search: Function Bottom: Return
       Variable rule.  This step verifies the function which was discovered by
       the Flow Check rule and directs the search into the function. *)
    | Side_effect_search_function_top of abstract_clause
    (* The Side Effect Search: Function Top rule.  The argument is the call site
       for function. *)
    | Side_effect_search_conditional_positive of annotated_clause
    (* The Side Effect Search: Conditional Positive rule.  The argument is the
       exit wiring for the conditional clause that triggered the search. *)
    | Side_effect_search_conditional_negative of annotated_clause
    (* The Side Effect Search: Conditional Positive rule.  The argument is the
       exit wiring for the conditional clause that triggered the search. *)
    | Side_effect_search_conditional_top
    (* The Side Effect Search: Conditional Top rule.  This rule merely lifts the
       appropriate side-effect lookup variable and removes the frame from
       beneath it. *)
    | Side_effect_search_function_wiring_join_defer_1_of_3
    (* The first step of the Side Effect Search: Function Wiring Join
       rule.  This step handles the conditional lookup variable as well as the
       parallel join symbol. *)
    | Side_effect_search_function_wiring_join_defer_2_of_3 of abstract_var
    (* The second step of the Side Effect Search: Function Wiring Join
       rule.  This step retrieves the store from the stack.  The argument here
       is the side-effect lookup variable captured from the previous step. *)
    | Side_effect_search_function_wiring_join_defer_3_of_3 of
        abstract_var * Abstract_store_witness_registry.escorted_witness
    (* The third step of the Side Effect Search: Function Wiring Join
       rule.  This step retrieves the trace concatenation from the stack.  The
       arguments here are the side-effect lookup variable captured from the
       first step and the store captured from the second step. *)
    | Side_effect_search_conditional_wiring_join_defer_1_of_2
    (* The first step of the Side Effect Search: Conditional Wiring Join Defer
       rule.  This step pops the side-effect lookup variable and the parallel
       join symbol. *)
    | Side_effect_search_conditional_wiring_join_defer_2_of_2 of abstract_var
    (* The second step of the Side Effect Search: Conditional Wiring Join Defer
       rule.  This step pops the store and the side-effect frame symbol.  The
       argument here is the variable popped from the first step. *)
    | Side_effect_search_join_compression_1_of_3
    (* The first step of the Side Effect Search: Join Compression rule.  This
       step pops the side-effect lookup variable and the first parallel join
       symbol. *)
    | Side_effect_search_join_compression_2_of_3 of abstract_var
    (* The second step of the Side Effect Search: Join Compression rule.  This
       step pops the first store and the second parallel join symbol.  The
       argument here is the variable popped in the first step. *)
    | Side_effect_search_join_compression_3_of_3 of
        abstract_var * Abstract_store_witness_registry.escorted_witness
    (* The third step of the Side Effect Search: Join Compression rule.  This
       step pops the second store and performs the join compression.  The
       arguments here are the variable popped in the first step and the store
       popped in the second step. *)
    | Side_effect_search_alias_analysis_start of
        annotated_clause * annotated_clause
    (* The Side Effect Search: Alias Analysis Start rule.  The provided
       clauses are the cell update which triggered the analysis and the
       clause immediately following it. *)
    (* The Alias Analysis Start rule.  This step retrieves the current lookup
       variable to set up the alias analysis process.  The arguments are the
       update clause, and the current clause (which is after the update
       clause). *)
    | Side_effect_search_may_not_alias_1_of_3
    (* The first step of the Side Effect Search: May Not Alias rule.  This step
       retrieves the first operand of the aliasing operation. *)
    | Side_effect_search_may_not_alias_2_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search: May Not Alias rule.  This step
       retrieves the second operand and waits for the lookup variable.  The
       argument here is the first operand. *)
    | Side_effect_search_may_not_alias_3_of_3 of
        Abstract_store_witness_registry.escorted_witness *
        Abstract_store_witness_registry.escorted_witness
    (* The third step of the Side Effect Search: May Not Alias rule.  This step
       retrieves the lookup variable and then performs the May Not Alias
       analysis. *)
    | Side_effect_search_may_alias_1_of_3 of abstract_var
    (* The first step of the Side Effect Search: May Alias rule.  This step
       retrieves the first operand of the aliasing operation.  The argument is
        the variable being assigned to the cell in the update operation. *)
    | Side_effect_search_may_alias_2_of_3 of
        abstract_var * Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search: May Alias rule.  This step
       retrieves the second operand and waits for the lookup variable.  The
       arguments are the variable being assigned to the cell and the first
       operand. *)
    | Side_effect_search_may_alias_3_of_3 of
        abstract_var *
        Abstract_store_witness_registry.escorted_witness *
        Abstract_store_witness_registry.escorted_witness
    (* The third step of the Side Effect Search: May Alias rule.  This step
       retrieves the lookup variable and then performs the May Alias analysis.
       The arguments are the variable being assigned to the cell and the two
       operands. *)
    | Side_effect_search_escape_frame
    (* The Side Effect Search Escape: Frame rule. *)
    | Side_effect_search_escape_variable_concatenation_1_of_2
    (* The first step of the Side Effect Search Escape: Variable Concatenation
       rule.  This step pops the store and the escape symbol. *)
    | Side_effect_search_escape_variable_concatenation_2_of_2 of
        Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search Escape: Variable Concatenation
       rule.  This step pops the trace concatenation and performs it. *)
    | Side_effect_search_escape_store_join_1_of_2
    (* The first step of the Side Effect Search Escape: Store Join rule.  This
       step pops the first operand as well as the escape and join symbols. *)
    | Side_effect_search_escape_store_join_2_of_2 of
        Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search Escape: Store Join rule.  This
       step pops the second operand and performs the join. *)
    | Side_effect_search_escape_complete_1_of_3
    (* The first step of the Side Effect Search Escape: Complete rule.  This
       step pops the store and the escape symbol. *)
    | Side_effect_search_escape_complete_2_of_3 of
        Abstract_store_witness_registry.escorted_witness
    (* The second step of the Side Effect Search Escape: Complete rule.  This
       step pops the side-effect start symbol.  The argument here is the store
       popped from the first step. *)
    | Side_effect_search_escape_complete_3_of_3 of
        Abstract_store_witness_registry.escorted_witness * annotated_clause
    (* The third step of the Side Effect Search Escape: Complete rule.  This
       step pops the original lookup variable and the deref symbol, replacing
       them with the result and moving to an appropriate position. *)
    | Side_effect_search_not_found_shallow_1_of_2
    (* The first step in the Side Effect Search: Not Found (Shallow) rule.  This
       step pops and discards the side-effect lookup variable to look for a
       start symbol beneath. *)
    | Side_effect_search_not_found_shallow_2_of_2
    (* The second step in the Side Effect Search: Not Found (Shallow) rule.
       This step pops and discards any start symbol. *)
    | Side_effect_search_not_found_deep_1_of_4
    (* The first step in the Side Effect Search: Not Found (Deep) rule.  This
       step pops and discards the side-effect lookup variable as well as the
       parallel join symbol. *)
    | Side_effect_search_not_found_deep_2_of_4
    (* The second step in the Side Effect Search: Not Found (Deep) rule.  This
       step pops and keeps the store. *)
    | Side_effect_search_not_found_deep_3_of_4 of
        Abstract_store_witness_registry.escorted_witness
    (* The third step in the Side Effect Search: Not Found (Deep) rule.  This
       step pops and discards the start symbol.  The argument here is the store
       popped from the second step. *)
    | Side_effect_search_not_found_deep_4_of_4 of
        Abstract_store_witness_registry.escorted_witness
    (* The fourth step in the Side Effect Search: Not Found (Deep) rule.  This
       step pops the lookup variable and its deref symbol and then reintroduces
       items onto the stack. *)
    | Binary_operation_stop_1_of_2 of abstract_var * binary_operator
    (* The first step of the Binary Operation Stop rule.  This step pops the
       first operand.  The arguments are the variable to which the result is
       assigned and the operator being applied. *)
    | Binary_operation_stop_2_of_2 of
        abstract_var * binary_operator *
        Abstract_store_witness_registry.escorted_witness
    (* The first step of the Binary Operation Stop rule.  This step pops the
       second operand and performs the operation.  The arguments are the
       variable to which the result is assigned, the operator being applied, and
       the first operand. *)
    | Unary_operation_stop of abstract_var * unary_operator
    (* The Unary Operation Stop rule.  This step pops the operand and performs
       the operation.  The arguments are the variable to which the result is
       assigned and the operator being applied. *)
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
