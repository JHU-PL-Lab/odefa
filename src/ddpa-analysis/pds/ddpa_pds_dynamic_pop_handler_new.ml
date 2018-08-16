open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
open Ddpa_context_stack;;
open Ddpa_graph;;
open Ddpa_utils;;
open Pds_reachability_types_stack;;

let logger = Logger_utils.make_logger "Ddpa_pds_dynamic_pop_handler";;
let lazy_logger = Logger_utils.make_lazy_logger "Ddpa_pds_dynamic_pop_handler";;

module Make
    (C : Context_stack)
    (S : (module type of Ddpa_pds_structure_types_new.Make(C)) with module C = C)
    (F : (module type of Ddpa_pds_dynamic_pop_functions.Make(C)(S))
     with module C = C
      and module S = S)
=
struct
  open S;;

  module Stack_element = Pds_continuation;;
  module State = Pds_state;;
  module Targeted_dynamic_pop_action = F.Targeted;;
  module Untargeted_dynamic_pop_action = F.Untargeted;;
  module Stack_action = Stack_action_constructor(Stack_element)(F.Targeted);;
  module Terminus = Terminus_constructor(State)(F.Untargeted);;

  open Stack_action.T;;
  open Terminus.T;;

  let targeted_result_to_stack_action result =
    match result with
    | F.Targeted.Continuation_result(continuation, _) ->
      [Pop_dynamic_targeted continuation]
    | F.Targeted.Value_result(stack_elements) ->
      List.map (fun se -> Push(se)) stack_elements
  ;;

  let perform_targeted_dynamic_pop element action =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () ->
         Printf.sprintf "perform_targeted_dynamic_pop (%s) (%s)"
           (Pds_continuation.show element)
           (F.Targeted.show action))
      (fun results ->
         String_utils.concat_sep_delim "[" "]" ", "
           (
             results
             |> Enum.clone
             |> Enum.map (String_utils.string_of_list Stack_action.show)
           )
      )
    @@ fun () ->
    F.Targeted.execute action element
    |> Enum.map
      (fun result ->
         match result with
         | F.Targeted.Continuation_result(continuation, _) ->
           [Pop_dynamic_targeted continuation]
         | F.Targeted.Value_result(stack_elements) ->
           List.map (fun se -> Push(se)) stack_elements
      )
  ;;

  let perform_untargeted_dynamic_pop element action =
    F.Untargeted.execute action element
    |> Enum.map
      (fun result ->
         match result with
         | F.Untargeted.Continuation_result(continuation) ->
           ([], Dynamic_terminus(continuation))
         | F.Untargeted.Value_result(stack_elements, target) ->
           (List.map (fun se -> Push(se)) stack_elements,
            Static_terminus(target)
           )
      )
  ;;

  let create_edge_function (edge : ddpa_edge) (state : S.pds_state)
    : (Stack_action.t list * Terminus.t) Enum.t =
    let open Nondeterminism.Nondeterminism_monad in
    enum @@
    let Ddpa_edge(acl1, acl0) = edge in
    let%orzero Program_point_state(acl0_,ctx) = state in
    [%guard equal_annotated_clause acl0 acl0_];
    let dynamic_pop_results =
      F.Targeted.start edge state
      |> Enum.map
        (fun result ->
           match result with
           | F.Targeted.Continuation_result(continuation, state_data_opt) ->
             let state' =
               match state_data_opt with
               | None -> state
               | Some(acl,ctx) -> Program_point_state(acl,ctx)
             in
             ([Pop_dynamic_targeted continuation], Static_terminus state')
           | F.Targeted.Value_result(stack_elements) ->
             (List.map (fun se -> Push(se)) stack_elements,
              Static_terminus state
             )
        )
    in
    let nop_results =
      (match acl1 with
       | Start_clause _ | End_clause _ ->
         Enum.singleton @@ Program_point_state(acl1,ctx)
       | _ -> Enum.empty ()
      )
      |> Enum.map
        (fun state -> ([], Static_terminus state))
    in
    pick_enum @@ Enum.append dynamic_pop_results nop_results
  ;;

  let create_untargeted_dynamic_pop_action_function
      (eobm : End_of_block_map.t) (edge : ddpa_edge) (state : S.pds_state)
    : Untargeted_dynamic_pop_action.t Enum.t =
    F.Untargeted.start eobm edge state
    |> Enum.filter_map
      (fun result ->
         match result with
         | F.Untargeted.Continuation_result(continuation) ->
           Some continuation
         | F.Untargeted.Value_result () ->
           None
      )
  ;;
end;;
