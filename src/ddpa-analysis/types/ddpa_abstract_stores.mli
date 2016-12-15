(**
   This module defines data structures and utilities pertaining to the abstract
   store types used by the analysis.
*)

open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
open Pds_reachability_utils;;

type relative_trace_part =
  | Trace_down of abstract_clause
  | Trace_up of abstract_clause
module Relative_trace_part : Decorated_type with type t = relative_trace_part
module Relative_trace : Decorated_type
module Relative_trace_var : Decorated_type
module Relative_trace_var_map : Map.S with type key = Relative_trace_var.t
module Raw_abstract_store : Decorated_type
module Abstract_store_root : Decorated_type
module Abstract_store : Decorated_type
module Abstract_store_set : sig
  include Set.S with type elt = Abstract_store.t
  include Pp_utils.Pp with type t := t
  include Yojson_utils.To_yojson_type with type t := t
end;;

val store_read : Abstract_store.t -> abstract_value
val store_is_variable_root : Abstract_store.t -> bool
val stores_have_same_root : Abstract_store.t -> Abstract_store.t -> bool

(** A module defining mechanisms for transforming stores. *)
module Ops :
sig
  (** The information necessary to construct store operations. *)
  module type Spec =
  sig
    val maximum_trace_length : int
  end;;
  (** The type of a module which can transform stores. *)
  module type Sig =
  sig
    val trace_suffix :
      Relative_trace.t option -> Relative_trace_part.t -> Relative_trace.t option
    val trace_concat :
      Relative_trace.t option -> Relative_trace.t option ->
      Relative_trace.t option
    val relative_trace_var_suffix :
      Relative_trace_var.t -> Relative_trace_part.t -> Relative_trace_var.t option
    val raw_store_suffix :
      Raw_abstract_store.t -> Relative_trace_part.t -> Raw_abstract_store.t
    val store_suffix_trace_part :
      Abstract_store.t -> Relative_trace_part.t -> Abstract_store.t
    val store_suffix_trace :
      Abstract_store.t -> Relative_trace.t -> Abstract_store.t
    val raw_store_join :
      Raw_abstract_store.t -> Raw_abstract_store.t -> Raw_abstract_store.t option
    val parallel_store_join :
      Abstract_store.t -> Abstract_store.t -> Abstract_store.t option
    val serial_store_join :
      Abstract_store.t -> Abstract_store.t -> Abstract_store.t option
    val store_singleton : abstract_var -> abstract_value -> Abstract_store.t
  end;;
  (** A functor to produce store operations. *)
  module Make(S : Spec) : Sig
end;;
