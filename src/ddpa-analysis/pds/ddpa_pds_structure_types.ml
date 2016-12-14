open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;
open Interface_utils;;

module type Bounded_capture_size_sig =
sig
  include Decorated_type;;
  val of_int : int -> t
  val equals_one : t -> bool
  val decrement : t -> t
end;;

module Make(Store_ops : Ddpa_abstract_stores.Ops.Sig) =
struct
  module Store_ops = Store_ops;;

  (** This module is meant to verify that the system never attempts to create
   *  a capture size larger than a fixed maximum (here, 4).  This property is
   *  necessary to argue that the analysis is decidable.
  *)
  module Bounded_capture_size : Bounded_capture_size_sig =
  struct
    type t = Bounded_capture_size of int [@@deriving eq, ord];;
    let max_capture_size = 8;;
    let of_int n =
      if n >= 1 && n <= max_capture_size
      then Bounded_capture_size(n)
      else raise @@ Utils.Invariant_failure(
          Printf.sprintf "Invalid size %d provided for bounded capture" n);;
    let equals_one (Bounded_capture_size(n)) = n = 1;;
    let decrement (Bounded_capture_size(n)) = Bounded_capture_size(n-1);;
    let pp formatter (Bounded_capture_size(n)) =
      Format.pp_print_int formatter n
    ;;
    let show = Pp_utils.pp_to_string pp;;
    let to_yojson (Bounded_capture_size n) = `Int n;;
  end;;

  type pds_continuation =
    | Bottom_of_stack
    (** The bottom of stack element is necessary as a sentinel. It's pushed as
        the initial element on the continuation stack so we don't need to check
        for empty continuation stacks. *)
    | Lookup_var of abstract_var
    | Project of ident
    | Deref
    | Continuation_store of Abstract_store.t
    | Real_flow_huh
    | Jump of annotated_clause
    | Capture of Bounded_capture_size.t
    | Alias_huh
    | Rewind
    | Unary_operation
    | Binary_operation
    | Parallel_join
    | Serial_join
    | Trace_concat of Relative_trace_part.t
    | Continuation_matches of pattern
    | Continuation_antimatches of pattern
    | Side_effect_search_start
    | Side_effect_frame
    | Side_effect_escape
    | Side_effect_lookup_var of abstract_var * annotated_clause
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
    | Program_point_state of annotated_clause
    (** A state in the PDS representing a specific program point and
        context. *)
    | Result_state of Abstract_store.t
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
