open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
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
    | Value_discovery(x,v) ->
      let%orzero Lookup_var x' = element in
      [%guard equal_abstract_var x x'];
      return [ Push(Continuation_store(Store_ops.store_singleton x v)) ]
    | Value_alias(x,x') ->
      let%orzero Lookup_var x0 = element in
      [%guard equal_abstract_var x x0];
      return [ Push(Lookup_var x') ]
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
