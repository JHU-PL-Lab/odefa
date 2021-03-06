open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_utils;;

open Ast;;
open Ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_context_stack;;
open Interface_utils;;

module type Bounded_capture_size_sig =
sig
  include Decorated_type;;
  val of_int : int -> t
  val to_int : t -> int
end;;

module Make(C : Context_stack) =
struct
  module C = C;;
  (** This module is meant to verify that the system never attempts to create
   *  a capture size larger than a fixed maximum (here, 4).  This property is
   *  necessary to argue that the analysis is decidable.
  *)
  module Bounded_capture_size : Bounded_capture_size_sig =
  struct
    type t = Bounded_capture_size of int [@@deriving eq, ord];;
    let max_capture_size = 5;;
    let of_int n =
      if n >= 1 && n <= max_capture_size
      then Bounded_capture_size(n)
      else raise @@ Utils.Invariant_failure(
          Printf.sprintf "Invalid size %d provided for bounded capture" n);;
    let to_int (Bounded_capture_size(n)) = n;;
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
    | Lookup_var of abstract_var * Pattern_set.t * Pattern_set.t
    | Project of ident * Pattern_set.t * Pattern_set.t
    | Jump of annotated_clause * C.t
    | Deref of Pattern_set.t * Pattern_set.t
    | Capture of Bounded_capture_size.t
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

  module Pds_state : Decorated_type with type t = pds_state =
  struct
    type t = pds_state [@@deriving eq, ord, show, to_yojson];;
  end;;

  type pds_state_class =
    | State_class_clause of annotated_clause
    | State_class_value
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Pds_state_class : Decorated_type with type t = pds_state_class =
  struct
    type t = pds_state_class [@@deriving eq, ord, show, to_yojson]
  end;;

  let classify (state : Pds_state.t) : Pds_state_class.t =
    match state with
    | Program_point_state(acl,_) -> State_class_clause acl
    | Result_state _ -> State_class_value
  ;;
end;;
