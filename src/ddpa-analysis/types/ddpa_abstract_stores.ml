open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
open Pds_reachability_utils;;
open Pp_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Ddpa_abstract_stores";;

type relative_trace_part =
  | Trace_enter of abstract_clause
  | Trace_exit of abstract_clause
[@@deriving eq, ord, to_yojson]
;;

let pp_relative_trace_part formatter t =
  match t with
  | Trace_enter c ->
    Format.fprintf formatter "◖%a" pp_abstract_clause_unique_info c
  | Trace_exit c ->
    Format.fprintf formatter "◗%a" pp_abstract_clause_unique_info c
;;

let show_relative_trace_part = pp_to_string pp_relative_trace_part;;

module Relative_trace_part =
struct
  type t = relative_trace_part;;
  let compare = compare_relative_trace_part;;
  let equal = equal_relative_trace_part;;
  let pp = pp_relative_trace_part;;
  let show = show_relative_trace_part;;
  let to_yojson = relative_trace_part_to_yojson;;
end;;

module Relative_trace_deque = Decorated_deque.Make(Relative_trace_part);;

type trace_state =
  | Full_trace
  | Partial_trace
[@@deriving eq, ord, to_yojson]
;;

type trace =
  | Trace of trace_state * Relative_trace_deque.t
[@@deriving eq, ord, to_yojson]
;;

module Relative_trace =
struct
  type t = trace;;
  let compare = compare_trace;;
  let equal = equal_trace;;
  let pp fmt t =
    let (Trace(state,parts)) = t in
    Format.pp_print_string fmt (if state = Partial_trace then "(" else "[");
    pp_concat_sep "" pp_relative_trace_part fmt @@
    Relative_trace_deque.enum parts;
    Format.pp_print_string fmt "]";
  ;;
  let show = pp_to_string pp;;
  let to_yojson = trace_to_yojson;;
  let empty = Trace(Full_trace, Relative_trace_deque.empty);;
end;;

type relative_trace_var =
  | Relative_trace_var of abstract_var * Relative_trace.t
[@@deriving eq, ord, to_yojson]
;;

let pp_relative_trace_var formatter (Relative_trace_var(x,t)) =
  pp_abstract_var formatter x;
  Format.pp_print_char formatter '@';
  Relative_trace.pp formatter t;
;;

let show_relative_trace_var = pp_to_string pp_relative_trace_var;;

module Relative_trace_var =
struct
  type t = relative_trace_var;;
  let compare = compare_relative_trace_var;;
  let equal = equal_relative_trace_var;;
  let pp = pp_relative_trace_var;;
  let show = show_relative_trace_var;;
  let to_yojson = relative_trace_var_to_yojson;;
end;;

let pp_abstract_value_brief fmt v =
  match v with
  | Abs_value_function(Abs_function_value(x,_)) ->
    Format.pp_print_string fmt "fun ";
    pp_abstract_var fmt x;
  | _ ->
    pp_abstract_value fmt v
;;

module Abstract_value_pp_brief : Decorated_type with type t = abstract_value =
struct
  type t = abstract_value
  let compare = compare_abstract_value
  let equal = equal_abstract_value;;
  let pp = pp_abstract_value_brief;;
  let show = pp_to_string pp;;
  let to_yojson = abstract_value_to_yojson;;
end;;

module Relative_trace_var_to_abstract_value_multimap =
struct
  module Impl = Multimap.Make(Relative_trace_var)(Abstract_value_pp_brief);;
  include Impl;;
  include Multimap_pp.Make(Impl)(Relative_trace_var)(Abstract_value_pp_brief);;
  include Multimap_to_yojson.Make(Impl)(Relative_trace_var)(Abstract_value_pp_brief);;
end;;

type raw_abstract_store =
  Relative_trace_var_to_abstract_value_multimap.t
[@@deriving eq, ord, show, to_yojson]
;;

ignore @@ show_raw_abstract_store;;

let show_raw_abstract_store = Pp_utils.pp_to_string pp_raw_abstract_store;;

module Raw_abstract_store : Decorated_type with type t = raw_abstract_store =
struct
  type t = raw_abstract_store
  let compare = compare_raw_abstract_store
  let equal = equal_raw_abstract_store
  let pp = pp_raw_abstract_store
  let show = show_raw_abstract_store
  let to_yojson = raw_abstract_store_to_yojson
end;;

type abstract_store_root =
  | Abstract_store_root of Relative_trace_var.t * Abstract_value_pp_brief.t
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_store_root formatter (Abstract_store_root(rx,v)) =
  Relative_trace_var.pp formatter rx;
  Format.pp_print_string formatter " ↦ ";
  Abstract_value_pp_brief.pp formatter v
;;

let show_abstract_store_root = pp_to_string pp_abstract_store_root;;

module Abstract_store_root =
struct
  type t = abstract_store_root
  let compare = compare_abstract_store_root
  let equal = equal_abstract_store_root
  let pp = pp_abstract_store_root
  let show = show_abstract_store_root
  let to_yojson = abstract_store_root_to_yojson
end;;

type abstract_store =
  { abstract_store_root : abstract_store_root
  ; raw_abstract_store : raw_abstract_store
  }
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_store formatter store =
  pp_tuple
    pp_abstract_store_root
    Raw_abstract_store.pp
    formatter
    ( store.abstract_store_root
    , store.raw_abstract_store
    )
;;

let show_abstract_store = pp_to_string pp_abstract_store;;

module Abstract_store =
struct
  type t = abstract_store
  let compare = compare_abstract_store
  let equal = equal_abstract_store
  let pp = pp_abstract_store
  let show = show_abstract_store
  let to_yojson = abstract_store_to_yojson
end;;

module Abstract_store_set =
struct
  module S = Set.Make(Abstract_store);;
  include S;;
  include Pp_utils.Set_pp(S)(Abstract_store);;
  include Yojson_utils.Set_to_yojson(S)(Abstract_store);;
end;;

let is_partial_trace (t : Relative_trace.t) =
  let Trace(state,_) = t in
  state = Partial_trace
;;

let destruct_relative_trace_var (rx : Relative_trace_var.t)
  : abstract_var * Relative_trace.t =
  let Relative_trace_var(rx,t) = rx in (rx,t)
;;

let store_read (s : abstract_store) : abstract_value =
  let Abstract_store_root(_,v) = s.abstract_store_root in
  v
;;

let store_is_unique_root (s : abstract_store) : bool =
  let Abstract_store_root(rx,_) = s.abstract_store_root in
  let Relative_trace_var(_,Trace(state,_)) = rx in
  state = Full_trace
;;

let stores_have_same_root (s1 : abstract_store) (s2 : abstract_store) : bool =
  equal_abstract_store_root s1.abstract_store_root s2.abstract_store_root
;;

let store_enum (s : abstract_store)
  : (Relative_trace_var.t * abstract_value) Enum.t =
  Relative_trace_var_to_abstract_value_multimap.enum s.raw_abstract_store
;;

module Ops =
struct
  (** The information necessary to construct store operations. *)
  module type Spec =
  sig
    val maximum_trace_length : int
  end;;
  (** The type of a module which can transform stores. *)
  module type Sig =
  sig
    module Exception : sig
      (** An exception raised by trace concatenation operations when a trace
          would generate an impossible stack operation sequence. *)
      exception Invalid_trace_concatenation;;

      (** Suffixes a part onto an existing trace.  If the resulting trace is
          invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val trace_suffix :
        Relative_trace.t -> Relative_trace_part.t -> Relative_trace.t

      (** Concatenates two traces.  If the resulting trace is invalid, the
          [Invalid_trace_concatenation] exception is raised. *)
      val trace_concat :
        Relative_trace.t -> Relative_trace.t -> Relative_trace.t

      (** Suffixes a trace part onto a relative trace variable.  If the
          resulting trace is invalid, the [Invalid_trace_concatenation]
          exception is raised. *)
      val relative_trace_var_suffix :
        Relative_trace_var.t -> Relative_trace_part.t -> Relative_trace_var.t

      (** Suffixes a trace part onto a raw store mapping.  If the resulting
          is invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val raw_store_mapping_suffix :
        Relative_trace_var.t * abstract_value -> Relative_trace_part.t ->
        Relative_trace_var.t * abstract_value

      (** Suffixes a trace part onto a raw store.  If the trace is invalid, the
          [Invalid_trace_concatenation] exception is raised. *)
      val raw_store_suffix :
        Raw_abstract_store.t -> Relative_trace_part.t -> Raw_abstract_store.t

      (** Suffixes a trace part onto a store.  If the trace is invalid, the
          [Invalid_trace_concatenation] exception is raised. *)
      val store_suffix_trace_part :
        Abstract_store.t -> Relative_trace_part.t -> Abstract_store.t

      (** Suffixes a trace onto a store.  If the trace is invalid, the
          [Invalid_trace_concatenation] exception is raised. *)
      val store_suffix_trace :
        Abstract_store.t -> Relative_trace.t -> Abstract_store.t
    end;;

    (** Suffixes a trace part onto a store.  If the trace is invalid, [None] is
        returned. *)
    val store_suffix_trace_part :
      Abstract_store.t -> Relative_trace_part.t -> Abstract_store.t option

    (** Suffixes a trace onto a store.  If the trace is invalid, [None] is
        returned. *)
    val store_suffix_trace :
      Abstract_store.t -> Relative_trace.t -> Abstract_store.t option

    (** Determines if two traces are consistent. *)
    val trace_consistent :
      Relative_trace.t -> Relative_trace.t -> bool

    (** Determines if two raw stores are trace-consistent. *)
    val trace_consistent_stores :
      Raw_abstract_store.t -> Raw_abstract_store.t -> bool

    (** Joins two raw stores.  If the stores contain inconsistent mappings,
                [None] is returned. *)
    val raw_store_join :
      Raw_abstract_store.t -> Raw_abstract_store.t ->
      Raw_abstract_store.t option

    (** Parallel joins two stores.  If the stores contain inconsistent
        mappings, [None] is returned. *)
    val parallel_store_join :
      Abstract_store.t -> Abstract_store.t -> Abstract_store.t option

    (** Serial joins two stores.  If the stores contain inconsistent mappings,
        [None] is returned. *)
    val serial_store_join :
      Abstract_store.t -> Abstract_store.t -> Abstract_store.t option

    (** Creates a singleton store. *)
    val store_singleton : abstract_var -> abstract_value -> Abstract_store.t
  end;;

  exception Merge_failure;;

  module Make(S : Spec) : Sig =
  struct
    module Exception =
    struct
      exception Invalid_trace_concatenation;;

      let trim_to_length (trace : Relative_trace.t) : Relative_trace.t =
        let Trace(_,parts) = trace in
        let num_parts = Relative_trace_deque.size parts in
        if num_parts > S.maximum_trace_length + 1 then
          raise (Utils.Invariant_failure(
              Printf.sprintf
                "Trace grew to size %d when maximum was %d"
                (Relative_trace_deque.size parts)
                (S.maximum_trace_length)
            ))
        else if num_parts > S.maximum_trace_length then
          let (_,parts') = Option.get @@ Relative_trace_deque.front parts in
          Trace(Partial_trace, parts')
        else
          trace
      ;;

      let trace_suffix (trace : Relative_trace.t) (part : relative_trace_part)
        : Relative_trace.t =
        let (Trace(state, parts)) = trace in
        match Relative_trace_deque.rear parts with
        | None ->
          (* Empty trace. *)
          if S.maximum_trace_length > 0
          then Trace(state, Relative_trace_deque.singleton part)
          else Trace(Partial_trace, Relative_trace_deque.empty)
        | Some (parts', part') ->
          begin
            match part', part with
            | Trace_enter c1, Trace_exit c2 ->
              if equal_abstract_clause c1 c2
              then
                (* In this case the two trace actions have cancelled each
                   other out and we can drop them. *)
                Trace(state, parts')
              else
                (* In this case, the two trace actions are incompatible!  This
                   stack operation sequence cannot exist and any operation
                   dependent upon it should halt. *)
                raise Invalid_trace_concatenation
            | _ ->
              let t = Trace(state, Relative_trace_deque.snoc parts part) in
              trim_to_length t
          end
      ;;

      let trace_concat (trace1 : Relative_trace.t) (trace2 : Relative_trace.t)
        : Relative_trace.t =
        let Trace(state2,parts2) = trace2 in
        match state2 with
        | Partial_trace ->
          trace2
        | Full_trace ->
          parts2
          |> Relative_trace_deque.enum
          |> Enum.fold trace_suffix trace1
      ;;

      type 'a relative_trace_mapper =
        (Relative_trace.t -> Relative_trace.t) -> 'a -> 'a
      ;;

      let relative_trace_var_trace_op :
        Relative_trace_var.t relative_trace_mapper =
        fun f rx ->
          let Relative_trace_var(x,trace) = rx in
          let trace' = f trace in
          Relative_trace_var(x,trace')
      ;;

      let raw_store_mapping_trace_op :
        (Relative_trace_var.t * abstract_value) relative_trace_mapper =
        fun f (rx,v) -> (relative_trace_var_trace_op f rx, v)
      ;;

      let raw_store_trace_op : raw_abstract_store relative_trace_mapper =
        fun f rs ->
          rs
          |> Relative_trace_var_to_abstract_value_multimap.enum
          |> Enum.map (raw_store_mapping_trace_op f)
          |> Relative_trace_var_to_abstract_value_multimap.of_enum
      ;;

      let store_trace_op : abstract_store relative_trace_mapper =
        fun f s ->
          let raw_abstract_store' = raw_store_trace_op f s.raw_abstract_store in
          let Abstract_store_root(rx,v) = s.abstract_store_root in
          let (rx',v') = raw_store_mapping_trace_op f (rx,v) in
          let abstract_store_root' = Abstract_store_root(rx',v') in
          { abstract_store_root = abstract_store_root'
          ; raw_abstract_store = raw_abstract_store'
          }
      ;;

      let relative_trace_var_suffix
          (rx : Relative_trace_var.t)
          (part : Relative_trace_part.t)
        : Relative_trace_var.t =
        relative_trace_var_trace_op (flip trace_suffix part) rx
      ;;

      let raw_store_mapping_suffix
          (m : Relative_trace_var.t * abstract_value)
          (part : Relative_trace_part.t)
        : Relative_trace_var.t * abstract_value =
        raw_store_mapping_trace_op (flip trace_suffix part) m
      ;;

      let raw_store_suffix
          (rs : raw_abstract_store)
          (part : relative_trace_part)
        : raw_abstract_store =
        raw_store_trace_op (flip trace_suffix part) rs
      ;;

      let store_suffix_trace_part
          (s : abstract_store)
          (part : relative_trace_part)
        : abstract_store =
        let answer = store_trace_op (flip trace_suffix part) s in
        lazy_logger `trace (fun () ->
            Printf.sprintf "Store suffix:\n  %s ⋉ %s = %s\n"
              (Abstract_store.show s) (Relative_trace_part.show part)
              (Abstract_store.show answer)
          );
        answer
      ;;

      let store_suffix_trace
          (s : abstract_store)
          (trace : Relative_trace.t)
        : abstract_store =
        store_trace_op (flip trace_concat trace) s
      ;;
    end;;

    let store_suffix_trace_part s p =
      try
        Some (Exception.store_suffix_trace_part s p)
      with
      | Exception.Invalid_trace_concatenation -> None
    ;;

    let store_suffix_trace s t =
      try
        Some (Exception.store_suffix_trace s t)
      with
      | Exception.Invalid_trace_concatenation -> None
    ;;

    let rec trace_consistent
        (trace1 : Relative_trace.t)
        (trace2 : Relative_trace.t)
      : bool =
      let Trace(_,parts1) = trace1 in
      let Trace(_,parts2) = trace2 in
      let rec trace_consistent_by_parts parts1 parts2 =
        match Deque.rear parts1, Deque.rear parts2 with
        | None, _
        | _, None ->
          (* One of them was empty, so we can't be inconsistent. *)
          true
        | Some(parts1', part1), Some(parts2', part2) ->
          match part1, part2 with
          | Trace_enter c1, Trace_enter c2 ->
            equal_abstract_clause c1 c2 &&
            trace_consistent_by_parts parts1' parts2'
          | Trace_enter _, Trace_exit _
          | Trace_exit _, Trace_enter _
          | Trace_exit _, Trace_exit _ ->
            (* One of them didn't have an enter component.  Applying an enter
               component is like popping from the hypothetical call stack and so
               is the only case when non-matching call sites might matter. *)
            true
      in
      trace_consistent_by_parts parts1 parts2
    ;;

    let rec trace_consistent_stores
        (rs1 : raw_abstract_store)
        (rs2 : raw_abstract_store)
      : bool =
      Enum.cartesian_product
        (Relative_trace_var_to_abstract_value_multimap.keys rs1)
        (Relative_trace_var_to_abstract_value_multimap.keys rs2)
      |> Enum.for_all
        (fun (rv1,rv2) ->
           let Relative_trace_var(_,trace1) = rv1 in
           let Relative_trace_var(_,trace2) = rv2 in
           trace_consistent trace1 trace2
        )
    ;;

    let raw_store_join
        (rs1 : raw_abstract_store)
        (rs2 : raw_abstract_store)
      : raw_abstract_store option =
      if trace_consistent_stores rs1 rs2 then
        let mappings, rs' =
          if Relative_trace_var_to_abstract_value_multimap.num_keys rs1 >
             Relative_trace_var_to_abstract_value_multimap.num_keys rs2 then
            (Relative_trace_var_to_abstract_value_multimap.enum_by_key rs2, rs1)
          else
            (Relative_trace_var_to_abstract_value_multimap.enum_by_key rs1, rs2)
        in
        try
          Some(
            mappings
            |> Enum.fold
              (fun acc (rx,vs) ->
                 let (Relative_trace_var(_,Trace(state,_))) = rx in
                 let acc' =
                   Relative_trace_var_to_abstract_value_multimap.S.enum vs
                   |> Enum.fold (fun a e ->
                       Relative_trace_var_to_abstract_value_multimap.add rx e a)
                     acc
                 in
                 begin
                   if state = Full_trace then
                     (* We'd best have only one mapping!  We know each of the
                        raw stores only has one mapping for each full trace by
                        induction, but we need to freak out here if they're
                        different. *)
                     let vs' =
                       Relative_trace_var_to_abstract_value_multimap.find
                         rx acc'
                     in
                     if Enum.count vs' > 1 then
                       raise Merge_failure
                 end;
                 acc'
              )
              rs'
          )
        with
        | Merge_failure -> None
      else
        None
    ;;

    let parallel_store_join
        (s1 : abstract_store) (s2 : abstract_store) : abstract_store option =
      let answer =
        match raw_store_join s1.raw_abstract_store s2.raw_abstract_store with
        | None -> None
        | Some rs -> Some { s1 with raw_abstract_store = rs }
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Parallel store join:\n  %s ⇇ %s = %s\n"
            (Abstract_store.show s1) (Abstract_store.show s2)
            (match answer with
             | None -> "☠"
             | Some rs -> Abstract_store.show rs)
        );
      answer
    ;;

    let serial_store_join
        (s1 : abstract_store) (s2 : abstract_store) : abstract_store option =
      let open Option.Monad in
      let answer =
        let Abstract_store_root(Relative_trace_var(_,trace),_) =
          s2.abstract_store_root
        in
        let%bind s1' = store_suffix_trace s1 trace in
        parallel_store_join s1' s2
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Serial store join:\n  %s ↫ %s = %s\n"
            (Abstract_store.show s1) (Abstract_store.show s2)
            (match answer with
             | None -> "☠"
             | Some rs -> Abstract_store.show rs)
        );
      answer
    ;;

    let store_singleton (x : abstract_var) (v : abstract_value)
      : abstract_store =
      let rx = Relative_trace_var(x, Relative_trace.empty) in
      { abstract_store_root = Abstract_store_root(rx,v)
      ; raw_abstract_store =
          Relative_trace_var_to_abstract_value_multimap.singleton rx v
      }
    ;;
  end;;
end;;

module Abstract_store_witness_registry =
struct
  module R = Witness_protection.Make_escorted(Abstract_store);;
  include R;;
  include Witness_protection.Make_pp(R)(Abstract_store);;
  include Witness_protection.Make_to_yojson(R)(Abstract_store);;
end;;
