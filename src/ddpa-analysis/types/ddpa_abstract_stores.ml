open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
open Interface_utils;;
open Pp_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Ddpa_abstract_stores";;

module Abstract_value_pp_brief : Decorated_type with type t = abstract_value =
struct
  type t = abstract_value
  let compare = compare_abstract_value
  let equal = equal_abstract_value;;
  let pp fmt v =
    match v with
    | Abs_value_function(Abs_function_value(x,_)) ->
      Format.pp_print_string fmt "fun ";
      pp_abstract_var fmt x;
    | _ ->
      pp_abstract_value fmt v
  ;;
  let show = pp_to_string pp;;
  let to_yojson = abstract_value_to_yojson;;
end;;

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

module Relative_trace_part_deque = Decorated_deque.Make(Relative_trace_part);;

type trace =
  | Trace of Relative_trace_part_deque.t
[@@deriving eq, ord, to_yojson]
;;

let pp_trace partial fmt trace =
  let (Trace(parts)) = trace in
  Format.pp_print_string fmt (if partial then "(" else "[");
  pp_concat_sep "" pp_relative_trace_part fmt @@
  Relative_trace_part_deque.enum parts;
  Format.pp_print_string fmt "]";
;;

module Relative_trace =
struct
  type t = trace;;
  let compare = compare_trace;;
  let equal = equal_trace;;
  let pp = pp_trace false;;
  let show = pp_to_string pp;;
  let to_yojson = trace_to_yojson;;
  let empty = Trace(Relative_trace_part_deque.empty);;
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

module Relative_trace_var_map =
struct
  module Impl = Map.Make(Relative_trace_var);;
  include Impl;;
  include Pp_utils.Map_pp(Impl)(Relative_trace_var);;
  include Yojson_utils.Map_to_yojson(Impl)(Relative_trace_var);;
end;;

type raw_abstract_store =
  abstract_value Relative_trace_var_map.t
[@@deriving eq, ord, to_yojson]
;;

let pp_raw_abstract_store =
  Pp_utils.pp_map
    Relative_trace_var.pp Abstract_value_pp_brief.pp Relative_trace_var_map.enum
;;

let show_raw_abstract_store = Pp_utils.pp_to_string pp_raw_abstract_store;;

module Raw_abstract_store =
struct
  type t = raw_abstract_store
  let compare = compare_raw_abstract_store
  let equal = equal_raw_abstract_store
  let pp = pp_raw_abstract_store
  let show = show_raw_abstract_store
  let to_yojson = raw_abstract_store_to_yojson
end;;

type root_precision =
  | Unique
  | Partial
[@@deriving eq, ord, to_yojson]
;;

type abstract_store_root =
  | Abstract_store_root of
      Relative_trace_var.t * abstract_value * root_precision
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_store_root formatter root =
  let Abstract_store_root(rx,v,precision) = root in
  begin
    match precision with
    | Unique ->
      Relative_trace_var.pp formatter rx;
    | Partial ->
      let Relative_trace_var(x,t) = rx in
      pp_abstract_var formatter x;
      Format.pp_print_string formatter "@";
      pp_trace true formatter t;
  end;
  Format.pp_print_string formatter " ↦ ";
  Abstract_value_pp_brief.pp formatter v;
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
  ; trace_enter_suffix : Relative_trace.t
  }
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_store formatter store =
  pp_triple
    pp_abstract_store_root
    Raw_abstract_store.pp
    Relative_trace.pp
    formatter
    ( store.abstract_store_root
    , store.raw_abstract_store
    , store.trace_enter_suffix
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

let store_read (s : abstract_store) : abstract_value =
  let Abstract_store_root(_,v,_) = s.abstract_store_root in
  v
;;

let store_is_unique_root (s : abstract_store) : bool =
  let Abstract_store_root(_,_,precision) = s.abstract_store_root in
  precision = Unique
;;

let stores_have_same_root (s1 : abstract_store) (s2 : abstract_store) : bool =
  equal_abstract_store_root s1.abstract_store_root s2.abstract_store_root
;;

let store_enum (s : abstract_store)
  : (Relative_trace_var.t * abstract_value) Enum.t =
  Relative_trace_var_map.enum s.raw_abstract_store
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

      (** Suffixes a part onto an existing trace.  The returned value is the
          resulting trace (trimmed to the maximum allowed by configuration) as
          well as a boolean indicating whether or not the trace was truncated
          (true if truncating occurred and false otherwise).  If the trace is
          invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val trace_suffix :
        Relative_trace.t -> Relative_trace_part.t -> Relative_trace.t * bool

      (** Concatenates two traces.  If the trace grows too long, it is truncated
          as per the [trace_suffix] routine.  The returned boolean indicates
          whether *any* truncating occurred.  If the trace is invalid, the
          [Invalid_trace_concatenation] exception is raised. *)
      val trace_concat :
        Relative_trace.t -> Relative_trace.t -> Relative_trace.t * bool

      (** Suffixes a trace part onto a relative trace variable.  If the trace
          grows too long, it is truncated as in [trace_suffix].  If the trace
          is invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val relative_trace_var_suffix :
        Relative_trace_var.t -> Relative_trace_part.t ->
        Relative_trace_var.t * bool

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

    val store_singleton : abstract_var -> abstract_value -> Abstract_store.t
  end;;

  exception Merge_failure;;

  module Make(S : Spec) : Sig =
  struct
    module Exception =
    struct
      exception Invalid_trace_concatenation;;

      let trace_suffix (trace : Relative_trace.t) (part : relative_trace_part)
        : Relative_trace.t * bool =
        let (Trace(parts)) = trace in
        match Relative_trace_part_deque.rear parts with
        | None ->
          (* Empty trace. *)
          if S.maximum_trace_length > 0
          then Trace(Relative_trace_part_deque.singleton part), false
          else Trace(Relative_trace_part_deque.empty), true
        | Some (parts', part') ->
          begin
            match part', part with
            | Trace_enter c1, Trace_exit c2 ->
              if equal_abstract_clause c1 c2
              then
                (* In this case the two trace actions have cancelled each
                   other out and we can drop them. *)
                Trace(parts'), false
              else
                (* In this case, the two trace actions are incompatible!  This
                   stack operation sequence cannot exist and any operation
                   dependent upon it should halt. *)
                raise Invalid_trace_concatenation
            | _ ->
              if Relative_trace_part_deque.size parts >= S.maximum_trace_length
              then
                (* In this case, the trace is too long and we need to trim the
                   front. *)
                let (_,parts'') =
                  Option.get @@ Relative_trace_part_deque.front parts
                in
                (Trace(Relative_trace_part_deque.snoc parts'' part)), true
              else
                (Trace(Relative_trace_part_deque.snoc parts part)), false
          end
      ;;

      let trace_concat (trace1 : Relative_trace.t) (trace2 : Relative_trace.t)
        : Relative_trace.t * bool =
        let Trace(parts2) = trace2 in
        let parts2enum = Relative_trace_part_deque.enum parts2 in
        let rec loop trace trunc =
          match Enum.get parts2enum with
          | None ->
            trace, trunc
          | Some part ->
            let (trace', trunc') = trace_suffix trace part in
            loop trace' (trunc || trunc')
        in
        loop trace1 false
      ;;

      let trim_trace_enter_suffix (trace : Relative_trace.t) =
        let Trace(parts) = trace in
        let rec loop parts' =
          match Relative_trace_part_deque.front parts' with
          | None
          | Some(Trace_enter _, _) -> parts'
          | Some(Trace_exit _, parts'') -> loop parts''
        in
        Trace(loop parts)
      ;;

      let max_enter_suffix
          (trace1 : Relative_trace.t)
          (trace2 : Relative_trace.t)
        : Relative_trace.t =
        let Trace(parts1) = trace1 in
        let Trace(parts2) = trace2 in
        let rec loop parts1 parts2 =
          match Relative_trace_part_deque.rear parts1,
                Relative_trace_part_deque.rear parts2 with
          | None, None -> trace1
          | Some _, None -> trace1
          | None, Some _ -> trace2
          | Some (parts1',part1), Some (parts2',part2) ->
            if equal_relative_trace_part part1 part2 then
              loop parts1' parts2'
            else
              raise Invalid_trace_concatenation
        in
        loop parts1 parts2
      ;;

      type 'a relative_trace_mapper =
        (Relative_trace.t -> Relative_trace.t * bool) -> 'a -> 'a * bool
      ;;

      let relative_trace_var_trace_op :
        Relative_trace_var.t relative_trace_mapper =
        fun f rx ->
          let Relative_trace_var(x,trace) = rx in
          let (trace',trunc) = f trace in
          (Relative_trace_var(x,trace')), trunc
      ;;

      let raw_store_mapping_trace_op :
        (Relative_trace_var.t * abstract_value) relative_trace_mapper =
        fun f (rx,v) ->
          let rx',trunc = relative_trace_var_trace_op f rx in
          (rx', v), trunc
      ;;

      let raw_store_trace_op :
        (Relative_trace.t -> Relative_trace.t * bool) -> raw_abstract_store ->
        raw_abstract_store
        =
        fun f rs ->
          rs
          |> Relative_trace_var_map.enum
          |> Enum.filter_map
            (fun m ->
               let mapping,trunc = raw_store_mapping_trace_op f m in
               if trunc then None else Some mapping
            )
          |> Relative_trace_var_map.of_enum
      ;;

      let relative_trace_var_suffix
          (rx : relative_trace_var)
          (part : relative_trace_part)
        : relative_trace_var * bool =
        relative_trace_var_trace_op (flip trace_suffix part) rx
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
        let answer =
          let raw_abstract_store' =
            raw_store_suffix s.raw_abstract_store part
          in
          let Abstract_store_root(rx,v,precision) = s.abstract_store_root in
          let (rx',v'),trunc =
            raw_store_mapping_trace_op (flip trace_suffix part) (rx,v)
          in
          let precision' =
            if precision = Unique && trunc = false then Unique else Partial
          in
          let abstract_store_root' = Abstract_store_root(rx',v',precision') in
          let trace_enter_suffix' =
            trim_trace_enter_suffix @@ fst @@
            trace_suffix s.trace_enter_suffix part
          in
          { raw_abstract_store = raw_abstract_store';
            abstract_store_root = abstract_store_root';
            trace_enter_suffix = trace_enter_suffix'
          }
        in
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
        let Trace(parts) = trace in
        parts
        |> Relative_trace_part_deque.enum
        |> Enum.fold store_suffix_trace_part s
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

    let raw_store_join
        (rs1 : raw_abstract_store)
        (rs2 : raw_abstract_store)
      : raw_abstract_store option =
      let merge_fn _ v1o v2o =
        match v1o,v2o with
        | None,None -> None
        | Some v1,None -> Some v1
        | None,Some v2 -> Some v2
        | Some v1,Some v2 ->
          if equal_abstract_value v1 v2
          then Some v1
          else raise Merge_failure
      in
      try
        Some (Relative_trace_var_map.merge merge_fn rs1 rs2)
      with
      | Merge_failure -> None
    ;;

    let parallel_store_join
        (s1 : abstract_store) (s2 : abstract_store) : abstract_store option =
      let answer =
        let sfx1 = s1.trace_enter_suffix in
        let sfx2 = s2.trace_enter_suffix in
        try
          let sfx' = Exception.max_enter_suffix sfx1 sfx2 in
          match raw_store_join s1.raw_abstract_store s2.raw_abstract_store with
          | None -> None
          | Some rs -> Some
                         { s1 with
                           raw_abstract_store = rs;
                           trace_enter_suffix = sfx'
                         }
        with
        | Exception.Invalid_trace_concatenation -> None
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
      let answer =
        let Abstract_store_root(Relative_trace_var(_,t2),_,precision2) =
          s2.abstract_store_root
        in
        match precision2 with
        | Partial ->
          (* The only meaningful information from the left side is its root
             variable and value.  The root trace is useless now, as are all of
             the mappings.  This kind of imprecision implies that the enter
             suffix from the left is useless as well. *)
          let Abstract_store_root(Relative_trace_var(x1,_),v1,_) =
            s1.abstract_store_root
          in
          let abstract_store_root' =
            Abstract_store_root(Relative_trace_var(x1,t2),v1,Partial)
          in
          Some
            { abstract_store_root = abstract_store_root';
              raw_abstract_store = s2.raw_abstract_store;
              trace_enter_suffix = s2.trace_enter_suffix;
            }
        | Unique ->
          let open Option.Monad in
          let Abstract_store_root(rx1,v1,precision1) =
            s1.abstract_store_root
          in
          let%bind rx',trunc_rx' =
            try
              Some(Exception.relative_trace_var_trace_op
                     (flip Exception.trace_concat t2) rx1)
            with
            | Exception.Invalid_trace_concatenation -> None
          in
          let precision' =
            if (precision1 = Unique) && not trunc_rx' then Unique else Partial
          in
          let abstract_store_root' = Abstract_store_root(rx',v1,precision') in
          let%bind trace_enter_suffix' =
            try
              Some(
                Exception.trim_trace_enter_suffix @@ fst @@
                Exception.trace_concat s1.trace_enter_suffix t2)
            with
            | Exception.Invalid_trace_concatenation -> None
          in
          let%bind raw_abstract_store' =
            try
              Some(
                Exception.raw_store_trace_op (flip Exception.trace_concat t2)
                  s1.raw_abstract_store
              )
            with
            | Exception.Invalid_trace_concatenation -> None
          in
          let s1' =
            { abstract_store_root = abstract_store_root';
              raw_abstract_store = raw_abstract_store';
              trace_enter_suffix = trace_enter_suffix' }
          in
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
      { abstract_store_root = Abstract_store_root(rx,v,Unique)
      ; raw_abstract_store = Relative_trace_var_map.singleton rx v
      ; trace_enter_suffix = Trace(Relative_trace_part_deque.empty)
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
