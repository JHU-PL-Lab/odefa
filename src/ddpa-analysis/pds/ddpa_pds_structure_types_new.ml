open Batteries;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_context_stack;;

module Make(C : Context_stack) =
struct
  module C = C;;

  type pds_continuation =
    | Bottom_of_stack
    (** The bottom of stack element is necessary as a sentinel. It's pushed as
        the initial element on the continuation stack so we don't need to check
        for empty continuation stacks. *)
    | Lookup_var of abstract_var * Pattern_set.t * Pattern_set.t
    | Project of ident * Pattern_set.t * Pattern_set.t
    | Jump of annotated_clause * C.t
    | Rewind
    | Deref of Pattern_set.t * Pattern_set.t
    | Capture1
    | Capture2
    | Capture3
    | Capture4
    | Capture5
    | Continuation_value of abs_filtered_value
    | Real_flow_huh
    | Alias_huh
    | Side_effect_search_start
    | Side_effect_search_escape of abstract_var
    | Side_effect_lookup_var of
        abstract_var * Pattern_set.t * Pattern_set.t * annotated_clause * C.t
    | Binary_operation
    | Unary_operation
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_continuation =
  struct
    type t = pds_continuation;;
    let equal = equal_pds_continuation;;
    let compare = compare_pds_continuation;;
    let pp = pp_pds_continuation;;
    let show = show_pds_continuation;;
    let to_yojson = pds_continuation_to_yojson;;
  end;;

  type pds_state =
    | Program_point_state of annotated_clause * C.t
    (** A state in the PDS representing a specific program point and
        context. *)
    | Result_state of abs_filtered_value
    (** A state in the PDS representing a value result. *)
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_state =
  struct
    type t = pds_state;;
    let equal = equal_pds_state;;
    let compare = compare_pds_state;;
    let pp = pp_pds_state;;
    let show = show_pds_state;;
    let to_yojson = pds_state_to_yojson;;
  end;;
end;;
