open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;

type relative_trace_part =
  | Trace_down of abstract_clause
  | Trace_up of abstract_clause
[@@deriving eq, ord, show, to_yojson]
;;

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
[@@deriving eq, ord, show, to_yojson]
;;

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
[@@deriving eq, ord, show, to_yojson]
;;

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
[@@deriving eq, ord, show, to_yojson]
;;

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
[@@deriving eq, ord, show, to_yojson]
;;

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

  exception Merge_failure;;

  module Make(S : Spec) : Sig =
  struct
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
            Some (Relative_trace.singleton part)
          | Some (trace', part') ->
            begin
              match part', part with
              | Trace_down c1, Trace_up c2
                when equal_abstract_clause c1 c2 ->
                Some trace'
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
    ;;

    let store_suffix_trace
        (s : abstract_store)
        (trace : Relative_trace.t)
      : abstract_store =
      trace
      |> Relative_trace.enum
      |> Enum.fold store_suffix_trace_part s
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
      match raw_store_join s1.raw_abstract_store s2.raw_abstract_store with
      | None -> None
      | Some rs -> Some { s1 with raw_abstract_store = rs }
    ;;

    let serial_store_join
        (s1 : abstract_store) (s2 : abstract_store) : abstract_store option =
      match s2.historical_trace with
      | None ->
        Some { abstract_store_root = Value_store_root (store_read s1)
             ; raw_abstract_store = s1.raw_abstract_store
             ; historical_trace = None
             }
      | Some historical_trace ->
        parallel_store_join (store_suffix_trace s1 historical_trace) s2
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
