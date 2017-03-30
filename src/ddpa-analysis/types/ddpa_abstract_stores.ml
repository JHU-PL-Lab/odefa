open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
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

module Relative_trace = Ddpa_deque.Make(Relative_trace_part);;

type relative_trace_var =
  | Relative_trace_var of abstract_var * Relative_trace.t
[@@deriving eq, ord, to_yojson]
;;

let pp_relative_trace_var formatter (Relative_trace_var(x,t)) =
  pp_abstract_var formatter x;
  pp_concat_sep "" pp_relative_trace_part formatter @@ Relative_trace.enum t
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

Relative_trace_var_map.pp;;

type raw_abstract_store =
  abstract_value Relative_trace_var_map.t
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_value_brief fmt v =
  match v with
  | Abs_value_function(Abs_function_value(x,_)) ->
    Format.pp_print_string fmt "fun ";
    pp_abstract_var fmt x;
  | _ ->
    pp_abstract_value fmt v
;;

let pp_raw_abstract_store =
  Pp_utils.pp_map
    Relative_trace_var.pp pp_abstract_value_brief Relative_trace_var_map.enum
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

type abstract_store_root =
  | Variable_store_root of relative_trace_var
  | Value_store_root of abstract_value
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_store_root formatter root =
  match root with
  | Variable_store_root rx ->
    pp_relative_trace_var formatter rx
  | Value_store_root v ->
    pp_abstract_value_brief formatter v
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
  ; historical_trace : Relative_trace.t option
  }
[@@deriving eq, ord, to_yojson]
;;

let pp_abstract_store formatter store =
  let pp_historical_trace formatter trace_opt =
    match trace_opt with
    | None -> Format.pp_print_string formatter "?"
    | Some trace -> Relative_trace.pp formatter trace
  in
  pp_triple
    pp_abstract_store_root
    Raw_abstract_store.pp
    pp_historical_trace
    formatter
    ( store.abstract_store_root
    , store.raw_abstract_store
    , store.historical_trace
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
  match s.abstract_store_root with
  | Variable_store_root rx ->
    Relative_trace_var_map.find rx s.raw_abstract_store
  | Value_store_root v ->
    v
;;

let store_is_variable_root (s : abstract_store) : bool =
  match s.abstract_store_root with
  | Variable_store_root _ -> true
  | Value_store_root _ -> false
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
      (** An exception raised by trace concatenation operations when a trace would
          generate an impossible stack operation sequence. *)
      exception Invalid_trace_concatenation;;

      (** Suffixes a part onto an existing trace.
          If the trace grows too long, [None] is returned.  If the trace is
          invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val trace_suffix :
        Relative_trace.t option -> Relative_trace_part.t ->
        Relative_trace.t option

      (** Concatenates two traces.
          If the trace grows too long, [None] is returned.  If the trace is
          invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val trace_concat :
        Relative_trace.t option -> Relative_trace.t option ->
        Relative_trace.t option

      (** Suffixes a trace part onto a relative trace variable.
          If the trace grows too long, [None] is returned.  If the trace is
          invalid, the [Invalid_trace_concatenation] exception is raised. *)
      val relative_trace_var_suffix :
        Relative_trace_var.t -> Relative_trace_part.t ->
        Relative_trace_var.t option

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

    val store_singleton : abstract_var -> abstract_value -> Abstract_store.t
  end;;

  exception Merge_failure;;

  module Make(S : Spec) : Sig =
  struct
    module Exception =
    struct
      exception Invalid_trace_concatenation;;

      let trace_suffix
          (trace_opt : Relative_trace.t option)
          (part : relative_trace_part)
        : Relative_trace.t option =
        match trace_opt with
        | None -> None
        | Some trace ->
          begin
            match Relative_trace.rear trace with
            | None ->
              (* Empty trace. *)
              if S.maximum_trace_length > 0
              then Some (Relative_trace.singleton part)
              else None
            | Some (trace', part') ->
              begin
                match part', part with
                | Trace_enter c1, Trace_exit c2 ->
                  if equal_abstract_clause c1 c2
                  then
                    (* In this case the two trace actions have cancelled each
                       other out and we can drop them. *)
                    Some trace'
                  else
                    (* In this case, the two trace actions are incompatible!  This
                       stack operation sequence cannot exist and any operation
                       dependent upon it should halt. *)
                    raise Invalid_trace_concatenation
                | _ ->
                  if Relative_trace.size trace >= S.maximum_trace_length
                  then None
                  else Some (Relative_trace.snoc trace part)
              end
          end
      ;;

      let trace_concat
          (trace_opt : Relative_trace.t option)
          (suffix_opt : Relative_trace.t option)
        : Relative_trace.t option =
        match trace_opt, suffix_opt with
        | None,None | None,Some _ | Some _,None ->
          None
        | Some _, Some suffix ->
          Relative_trace.enum suffix
          |> Enum.fold trace_suffix trace_opt
      ;;

      let relative_trace_var_suffix
          (rx : relative_trace_var)
          (part : relative_trace_part)
        : relative_trace_var option =
        let Relative_trace_var(x,trace) = rx in
        match trace_suffix (Some trace) part with
        | None -> None
        | Some trace' -> Some (Relative_trace_var(x,trace'))
      ;;

      let raw_store_suffix
          (rs : raw_abstract_store)
          (part : relative_trace_part)
        : raw_abstract_store =
        rs
        |> Relative_trace_var_map.enum
        |> Enum.filter_map
          (fun (rx,v) ->
             match relative_trace_var_suffix rx part with
             | None -> None
             | Some rx' -> Some (rx',v)
          )
        |> Relative_trace_var_map.of_enum
      ;;

      let store_suffix_trace_part
          (s : abstract_store)
          (part : relative_trace_part)
        : abstract_store =
        let answer =
          let raw_abstract_store' = raw_store_suffix s.raw_abstract_store part in
          let historical_trace' = trace_suffix s.historical_trace part in
          let abstract_store_root' =
            match s.abstract_store_root with
            | Value_store_root _ -> s.abstract_store_root
            | Variable_store_root rx ->
              (* If the root was a variable, then we extend that variable.  If this
                 extension truncates the variable, then we look up the *old* variable
                 in the *old* store and replace it with the corresponding value
                 root. *)
              begin
                match relative_trace_var_suffix rx part with
                | Some rx' -> Variable_store_root rx'
                | None ->
                  let v = Relative_trace_var_map.find rx s.raw_abstract_store in
                  Value_store_root v
              end
          in
          { abstract_store_root = abstract_store_root'
          ; raw_abstract_store = raw_abstract_store'
          ; historical_trace = historical_trace'
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
        trace
        |> Relative_trace.enum
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

    let rec trace_consistent
        (trace1 : Relative_trace.t)
        (trace2 : Relative_trace.t)
      : bool =
      match Deque.rear trace1, Deque.rear trace2 with
      | None, _
      | _, None ->
        (* One of them was empty, so we can't be inconsistent. *)
        true
      | Some(trace1', part1), Some(trace2', part2) ->
        match part1, part2 with
        | Trace_enter c1, Trace_enter c2 ->
          equal_abstract_clause c1 c2 && trace_consistent trace1' trace2'
        | Trace_enter _, Trace_exit _
        | Trace_exit _, Trace_enter _
        | Trace_exit _, Trace_exit _ ->
          (* One of them didn't have an enter component.  Applying an enter
             component is like popping from the hypothetical call stack and so
             is the only case when non-matching call sites might matter. *)
          true
    ;;

    let rec trace_consistent_stores
        (rs1 : raw_abstract_store)
        (rs2 : raw_abstract_store)
      : bool =
      Enum.cartesian_product
        (Relative_trace_var_map.keys rs1)
        (Relative_trace_var_map.keys rs2)
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
        if trace_consistent_stores rs1 rs2 then
          Some (Relative_trace_var_map.merge merge_fn rs1 rs2)
        else
          None
      with
      | Merge_failure -> None
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
      let zero () = None in
      let answer =
        match s2.historical_trace with
        | None ->
          return { abstract_store_root = Value_store_root (store_read s1)
                 ; raw_abstract_store = s2.raw_abstract_store
                 ; historical_trace = None
                 }
        | Some historical_trace ->
          let%orzero Some s1' = store_suffix_trace s1 historical_trace in
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
      { abstract_store_root = Variable_store_root rx
      ; raw_abstract_store = Relative_trace_var_map.singleton rx v
      ; historical_trace = Some Relative_trace.empty
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
