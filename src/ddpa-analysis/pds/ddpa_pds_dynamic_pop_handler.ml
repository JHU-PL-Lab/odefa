open Batteries;;
open Jhupllib;;

open Core_ast;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;
open Ddpa_utils;;
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
    | Store_suffix_1_of_2 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted(Store_suffix_2_of_2 s) ]
    | Store_suffix_2_of_2 s ->
      let%orzero Trace_concat p = element in
      let s' = Store_ops.store_suffix_trace_part s p in
      return [ Push(Continuation_store s') ]
    | Store_parallel_join_1_of_3 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted(Store_parallel_join_2_of_3 s) ]
    | Store_parallel_join_2_of_3 s ->
      let%orzero Parallel_join = element in
      return [ Pop_dynamic_targeted(Store_parallel_join_3_of_3 s) ]
    | Store_parallel_join_3_of_3 s1 ->
      let%orzero Continuation_store s2 = element in
      let%orzero Some s' = Store_ops.parallel_store_join s1 s2 in
      return [ Push(Continuation_store s') ]
    | Store_serial_join_1_of_3 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted(Store_serial_join_2_of_3 s) ]
    | Store_serial_join_2_of_3 s ->
      let%orzero Serial_join = element in
      return [ Pop_dynamic_targeted(Store_serial_join_3_of_3 s) ]
    | Store_serial_join_3_of_3 s1 ->
      let%orzero Continuation_store s2 = element in
      let%orzero Some s' = Store_ops.serial_store_join s1 s2 in
      return [ Push(Continuation_store s') ]
    | Stateless_clause_skip_1_of_2 x' ->
      let%orzero Lookup_var x = element in
      [%guard not @@ equal_abstract_var x x'];
      return [ Pop_dynamic_targeted(Stateless_clause_skip_2_of_2 x) ]
    | Stateless_clause_skip_2_of_2 x ->
      begin
        match element with
        | Deref -> zero ()
        | _ -> return [ Push element; Push(Lookup_var x) ]
      end
    | Capture_1_of_3 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted(Capture_2_of_3 s) ]
    | Capture_2_of_3 s ->
      let%orzero Capture n = element in
      return [ Pop_dynamic_targeted(Capture_3_of_3(s,n,[])) ]
    | Capture_3_of_3(s,n,ks) ->
      if Struct.Bounded_capture_size.equals_one n
      then
        let n' = Struct.Bounded_capture_size.decrement n in
        return [ Pop_dynamic_targeted(Capture_3_of_3(s,n',element::ks)) ]
      else
        let ks' = element::ks in
        let ks'' = (Continuation_store s)::ks' in
        return @@ List.map (fun x -> Push x) ks''
    | Function_bottom_return_variable_1_of_2(x,x',c) ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted(
          Function_bottom_return_variable_2_of_2(x,x',c,s)) ]
    | Function_bottom_return_variable_2_of_2(x,x',c,s) ->
      let%orzero Lookup_var x0 = element in
      [%guard equal_abstract_var x x0];
      let%orzero Abs_value_function(Abs_function_value(_,e)) = store_read s in
      let Abs_expr cls = e in
      [%guard equal_abstract_var x' (rv @@ cls)];
      return [ Push (Continuation_store s)
             ; Push Parallel_join
             ; Push (Trace_concat (Trace_up c))
             ; Push (Lookup_var x')
             ]
    | Function_top_nonlocal_variable(x'',c) ->
      let%orzero Lookup_var x = element in
      [%guard not @@ equal_abstract_var x x'' ];
      let%orzero Abs_clause(_,Abs_appl_body(x2'',_)) = c in
      return [ Push (Trace_concat (Trace_down c))
             ; Push Serial_join
             ; Push (Lookup_var x)
             ; Push Rewind
             ; Push (Capture (Struct.Bounded_capture_size.of_int 3))
             ; Push (Lookup_var x2'')
             ]
    | Conditional_top_nonsubject_variable(x',x1) ->
      let%orzero Lookup_var x = element in
      [%guard not @@ equal_abstract_var x x'];
      [%guard not @@ equal_abstract_var x x1];
      return [ Push element ]
    | Record_projection_stop_1_of_2 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted (Record_projection_stop_2_of_2 s) ]
    | Record_projection_stop_2_of_2 s ->
      let%orzero Project l = element in
      let%orzero Abs_value_record(Abs_record_value(r)) = store_read s in
      let%orzero Some x' = Ident_map.Exceptionless.find l r in
      return [ Push (Continuation_store s)
             ; Push Serial_join
             ; Push (Lookup_var x')
             ]
    | Filter_immediate_1_of_2 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted (Filter_immediate_2_of_2 s) ]
    | Filter_immediate_2_of_2 s ->
      let%bind should_immediately_match, p =
        match element with
        | Continuation_matches p -> return (true,p)
        | Continuation_antimatches p -> return (false,p)
        | _ -> zero ()
      in
      let%bind immediate_match =
        begin
          match p with
          | Record_pattern r when Ident_map.is_empty r ->
            begin
              match store_read s with
              | Abs_value_record _ -> return true
              | _ -> return false
            end
          | _ ->
            let%orzero Some acceptable =
              immediately_matched_by @@ store_read s
            in
            return @@ Pattern_set.mem p acceptable
        end
      in
      [%guard immediate_match = should_immediately_match];
      return [ Push (Continuation_store s) ]
    | Filter_nonempty_record_positive_1_of_2 acl0 ->
      let%orzero Continuation_store s = element in
      let%orzero Abs_value_record _ = store_read s in
      return [ Pop_dynamic_targeted
                 (Filter_nonempty_record_positive_2_of_2 (acl0,s)) ]
    | Filter_nonempty_record_positive_2_of_2 (acl0,s) ->
      let%orzero Continuation_matches(Record_pattern pr) = element in
      let%orzero Abs_value_record(Abs_record_value vr) = store_read s in
      [%guard not @@ Ident_map.is_empty pr];
      let (lbl,p1),pr' = Ident_map.pop_min_binding pr in
      [%guard Ident_map.mem lbl vr];
      let%orzero Some x1 = Ident_map.Exceptionless.find lbl vr in
      let rest_of_pattern = Record_pattern pr' in
      return [ Push Parallel_join
             ; Push (Continuation_matches rest_of_pattern)
             ; Push (Continuation_store s)
             ; Push (Jump acl0)
             ; Push (Capture (Struct.Bounded_capture_size.of_int 4))
             ; Push (Continuation_matches p1)
             ; Push (Lookup_var x1)
             ]
    | Filter_nonempty_record_negative_1_of_2 acl0 ->
      let%orzero Continuation_store s = element in
      let%orzero Abs_value_record _ = store_read s in
      return [ Pop_dynamic_targeted
                 (Filter_nonempty_record_negative_2_of_2 (acl0,s)) ]
    | Filter_nonempty_record_negative_2_of_2 (acl0,s) ->
      let%orzero Continuation_matches(Record_pattern pr) = element in
      let%orzero Abs_value_record(Abs_record_value vr) = store_read s in
      [%guard not @@ Ident_map.is_empty pr];
      if Ident_map.for_all (fun i _ -> Ident_map.mem i vr) pr
      then
        (* Every label in the record pattern is in the record value somewhere.
           We just have to find *some* label for which we can refute the
           pattern.  Let's pick a label wlog.  :) *)
        let%bind label = pick_enum @@ Ident_map.keys pr in
        return [ Push Parallel_join
               ; Push (Continuation_store s)
               ; Push (Jump acl0)
               ; Push (Capture (Struct.Bounded_capture_size.of_int 3))
               ; Push (Continuation_antimatches (Ident_map.find label pr))
               ; Push (Lookup_var (Ident_map.find label vr))
               ]
      else
        (* There's at least one label in the pattern which isn't in the record,
           so we've definitely refuted this pattern. *)
        return [ Push (Continuation_store s) ]
    | Dereference_stop ->
      let%orzero Continuation_store s = element in
      let%orzero Abs_value_ref(Abs_ref_value(x')) = store_read s in
      return [ Pop Deref
             ; Push (Continuation_store s)
             ; Push Serial_join
             ; Push (Lookup_var x')
             ]
    | Alias_analysis_start(acl1,acl0) ->
      let%orzero Lookup_var x = element in
      let%orzero
        Unannotated_clause(Abs_clause(_,Abs_update_body(x1',_))) = acl1
      in
      return [ Pop Deref
             ; Push Deref
             ; Push (Lookup_var x)
             ; Push Alias_huh
             ; Push (Jump acl0)
             ; Push (Capture (Struct.Bounded_capture_size.of_int 2))
             ; Push (Lookup_var x1')
             ; Push (Jump acl1)
             ; Push (Capture (Struct.Bounded_capture_size.of_int 5))
             ; Push (Lookup_var x)
             ]
    | May_not_alias_1_of_3 ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted (May_not_alias_2_of_3 s) ]
    | May_not_alias_2_of_3 s1 ->
      let%orzero Continuation_store s2 = element in
      return [ Pop_dynamic_targeted (May_not_alias_3_of_3(s1,s2)) ]
    | May_not_alias_3_of_3 (s1,s2) ->
      let%orzero Lookup_var x = element in
      let%orzero Some s3 = Store_ops.parallel_store_join s1 s2 in
      let s3_is_ref =
        begin
          match store_read s3 with
          | Abs_value_ref _ -> true
          | _ -> false
        end
      in
      if store_is_variable_root s1 &&
         store_is_variable_root s2 &&
         s3_is_ref
      then
        (* In this case, it *is* an alias and this rule gives up. *)
        zero ()
      else
        (* There's some reason that this ref may not be an alias of the one
           we're seeking.  Keep moving, but remember the constraints we've
           accumulated. *)
        return [ Pop Deref
               ; Push (Continuation_store s3)
               ; Push Parallel_join
               ; Push Deref
               ; Push (Lookup_var x)
               ]
    | May_alias_1_of_3 x2' ->
      let%orzero Continuation_store s = element in
      return [ Pop_dynamic_targeted (May_alias_2_of_3(x2',s)) ]
    | May_alias_2_of_3(x2',s1) ->
      let%orzero Continuation_store s2 = element in
      return [ Pop_dynamic_targeted (May_alias_3_of_3(x2',s1,s2)) ]
    | May_alias_3_of_3(x2',s1,s2) ->
      let%orzero Lookup_var _ = element in
      let%bind () =
        if store_is_variable_root s1 &&
           store_is_variable_root s2 &&
           stores_have_same_root s1 s2
        then
          let%orzero Abs_value_ref(Abs_ref_value(x''s1)) = store_read s1 in
          let%orzero Abs_value_ref(Abs_ref_value(x''s2)) = store_read s2 in
          [%guard equal_abstract_var x''s1 x''s2]; return ()
        else
          return ()
      in
      return [ Pop Deref
             ; Push (Lookup_var x2')
             ]
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
    | Do_rewind acl ->
      let%orzero Rewind = element in
      return ([], Program_point_state acl)
  ;;
end;;
