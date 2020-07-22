(*
The implementation of this monad includes the following features:

  * Suspension via coroutine
  * Logging via writer (with sanity checking via a "listen"-like mechanism)
  * Nondeterminism via lists
  * State for caching common computations

Note that nondeterminism doesn't play nicely with most other features.  Using
transformers, it is possible to produce nondeterminism *independent* of other
features, but we want the features to interact.  In the event that incoherent
logs are written (e.g. conflicting decisions in function wiring choices), for
example, we want computation to zero.  This requires us to customize the monad
rather than relying on transformer definitions.
*)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Interpreter_types;;

let lazy_logger =
  Logger_utils.make_lazy_logger "Symbolic_monad"
;;
let _ = lazy_logger;; (* to suppress unused warning *)

type ('cache_key, 'work) work_info = {
  work_item : 'work;
  work_cache_key : 'cache_key option;
};;

module type Cache_key = sig
  include Gmap.KEY;;
  type some_key = Some_key : 'a t -> some_key;;
  val pp : 'a t Jhupllib.Pp_utils.pretty_printer;;
  val show : 'a t -> string;;
end;;

module type WorkCollection = sig
  module Work_cache_key : Cache_key;;
  type 'a t;;
  val empty : 'a t;;
  val is_empty : 'a t -> bool;;
  val size : 'a t -> int;;
  val offer : (Work_cache_key.some_key, 'a) work_info -> 'a t -> 'a t;;
  val take : 'a t -> ((Work_cache_key.some_key, 'a) work_info * 'a t) option;;
  val enum : 'a t -> 'a Enum.t
end;;

module QueueWorkCollection(Cache_key : Cache_key)
  : WorkCollection with module Work_cache_key = Cache_key =
struct
  module Work_cache_key = Cache_key;;
  type 'a t = (Work_cache_key.some_key, 'a) work_info Deque.t;;
  let empty = Deque.empty;;
  let is_empty = Deque.is_empty;;
  let size = Deque.size;;
  let offer info dq = Deque.snoc dq info;;
  let take dq = Deque.front dq;;
  let enum dq = Enum.map (fun w -> w.work_item) @@ Deque.enum dq;;
end;;

module CacheKeyPriorityQueueWorkCollection
    (Cache_key : Cache_key)
    (Priority : Interfaces.OrderedType with type t = Cache_key.some_key option)
  : WorkCollection with module Work_cache_key = Cache_key =
struct
  module Work_cache_key = Cache_key;;
  module KPQ = Priority_queue.Make(Priority);;
  type 'a t = (Work_cache_key.some_key, 'a) work_info KPQ.t;;
  let empty = KPQ.empty;;
  let is_empty pq = KPQ.size pq = 0;;
  let size = KPQ.size;;
  let offer info pq = KPQ.enqueue info.work_cache_key info pq;;
  let take pq =
    if KPQ.is_empty pq then None else
      match KPQ.dequeue pq with
      | None -> None
      | Some(_,v,pq') -> Some(v,pq')
  ;;
  let enum pq = Enum.map (fun w -> w.work_item) @@ KPQ.enum pq;;
end;;

module type Spec = sig
  module Cache_key : Cache_key;;
  module Work_collection
    : WorkCollection with module Work_cache_key = Cache_key;;
end;;

module type S = sig
  module Spec : Spec;;

  type 'a m;;

  val return : 'a -> 'a m;;
  val bind : 'a m -> ('a -> 'b m) -> 'b m;;
  val zero : unit -> 'a m;;
  val pick : 'a Enum.t -> 'a m;;
  val pause : unit -> unit m;;
  val cache : 'a Spec.Cache_key.t -> 'a m -> 'a m;;
  val cache_abort : 'a Spec.Cache_key.t -> 'a m -> 'a m;;
  val record_decision :
    Relative_stack.t -> Ident.t -> clause -> Ident.t -> unit m;;
  val record_constraint : Constraint.t -> unit m;;
  val record_abort_point : symbol -> abort_value -> unit m;;
  val check_constraints : 'a m -> 'a m;;

  type 'a evaluation;;

  type 'a evaluation_result =
    { er_value : 'a;
      er_solver : Solver.t;
      er_abort_points : abort_value Symbol_map.t;
      er_evaluation_steps : int;
      er_result_steps : int;
    };;

  val start : 'a m -> 'a evaluation;;
  val step :
    ?show_value:('a -> string) ->
    'a evaluation ->
    'a evaluation_result Enum.t * 'a evaluation;;
  val is_complete : 'a evaluation -> bool;;
end;;

(** The interface of the functor producing symbolic monads. *)
module Make(Spec : Spec) : S with module Spec = Spec
=
struct
  module Spec = Spec;;
  open Spec;;

  (* **** Supporting types **** *)

  type decision = Ident.t * clause * Ident.t [@@deriving show];;
  let _ = show_decision;;

  type decision_map =(Ident.t * clause * Ident.t) Relative_stack.Map.t
  [@@deriving show];;
  let _ = show_decision_map;;

  type log = {
    log_solver : Solver.t;
    log_decisions : decision_map;
    log_abort_points : abort_value Symbol_map.t;
    log_steps : int;
  }
  [@@deriving show];;
  let _ = show_log;;

  (* Not currently using state. *)
  type state = unit;;

  (* **** Monad types **** *)

  type 'a m = M of (state -> 'a blockable list * state)

  (** An unblocked value is either a completed expression (with its resulting
      state) or a suspended function which, when invoked, will step to the next
      result.  The suspended case carries the log of the computation at the time
      it was suspended. *)
  and 'a unblocked =
    | Completed : 'a * log -> 'a unblocked
    | Suspended : 'a m * log -> 'a unblocked

  (** A blocked value is a function waiting on the completion of a to-be-cached
      computation.  It retains the key of the computation it needs to have
      completed, the function which will use that value to unblock itself, and
      the log at the time computation was suspended.  Since a given computation
      may, via binding, block on many values, the function returns a blockable.
      Note that, although the computation is nondeterministic, each thread of
      computation is serial; there is a fixed order in which blocked values
      require their cached results, so we only wait for one value at a time. *)
  and ('a, 'b) blocked =
    { blocked_key : 'a Cache_key.t;
      blocked_consumer : ('a * log) -> 'b m;
      blocked_computation : 'a m;
    }

  (** A blockable value is either a blocked value or an unblocked value. *)
  and 'a blockable =
    | Blocked : ('z, 'a) blocked -> 'a blockable
    | Unblocked : 'a unblocked -> 'a blockable
  ;;

  (* **** Log utilities **** *)

  (* A log contains the constraint set (phi in the paper, solver here) and
     the set of decision paths (pi in the paper, decisions here). We also
     count the number of steps the algorithm takes. - KQ *)
  let empty_log = {
    log_solver = Solver.empty;
    log_decisions = Relative_stack.Map.empty;
    log_abort_points = Symbol_map.empty;
    log_steps = 0;
  }
  ;;

  exception MergeFailure of
      Relative_stack.t * (ident * clause * ident) * (ident * clause * ident);;

  exception MergeAbortsFailure of
      symbol * abort_value * abort_value;;
  
  let _trace_log_solver_contradiction
      (log1: log)
      (log2: log)
      (contradiction : Solver.contradiction)
    : state =
    let msg = begin
    match contradiction with
    | Solver.TypeContradiction(symbol, t1, t2) ->
        Printf.sprintf
          "Immediate contradiction at symbol %s with types %s and %s while merging two formula sets.\nSet 1:\n%s\nSet 2:\n%s\n"
          (show_symbol symbol)
          (Constraint.show_symbol_type t1)
          (Constraint.show_symbol_type t2)
          (Solver.show log1.log_solver)
          (Solver.show log2.log_solver)
    | Solver.ValueContradiction(symbol, v1, v2) ->
        Printf.sprintf
          "Immediate contradiction at symbol %s with values %s and %s while merging two formula sets.\nSet 1:\n%s\nSet 2:\n%s\n"
          (show_symbol symbol)
          (Constraint.show_value v1)
          (Constraint.show_value v2)
          (Solver.show log1.log_solver)
          (Solver.show log2.log_solver)
    | Solver.ProjectionContradiction(s1, s2, lbl) ->
        Printf.sprintf
          "Immediate contradiction at symbol %s by projection of label %s from symbol %s while merging two formula sets.\nSet 1:\n%s\nSet 2:\n%s\n"
          (show_symbol s1)
          (show_ident lbl)
          (show_symbol s2)
          (Solver.show log1.log_solver)
          (Solver.show log2.log_solver)
    | Solver.MatchContradiction(s1, s2, p) ->
        Printf.sprintf
          "Immediate contradiction at symbol %s by matching %s on %s while merging two formula sets.\nSet1:\n%s\nSet 2:\n%s\n"
          (show_symbol s1)
          (show_symbol s2)
          (Pp_utils.pp_to_string pp_pattern p)
          (Solver.show log1.log_solver)
          (Solver.show log2.log_solver)
    | Solver.StackContradiction(s1, s2) ->
        Printf.sprintf
          "Immediate contradiction of flows which resolve to different concrete stacks %s and %s.\nSet 1:\n%s\nSet 2:\n%s\n"
          (Relative_stack.show_concrete_stack s1)
          (Relative_stack.show_concrete_stack s2)
          (Solver.show log1.log_solver)
          (Solver.show log2.log_solver)
    end
    in lazy_logger `trace @@ fun () -> msg
  ;;

  let _trace_log_decision_contradiction key v1 v2 : state =
    let show_v = 
      Pp_utils.pp_to_string
        (Pp_utils.pp_triple pp_ident Ast_pp_brief.pp_clause pp_ident)
    in
    lazy_logger `trace (fun () ->
      Printf.sprintf
        "Contradiction while merging %s decisions: %s and %s"
        (Relative_stack.show key)
        (show_v v1)
        (show_v v2)
    )
  ;;

  let _merge_two_decisions key a b =
    match a, b with
    | None, None -> None
    | Some x, None -> Some x
    | None, Some x -> Some x
    | Some((x1, c1, x1') as v1), Some((x2, c2, x2') as v2) ->
      if equal_ident x1 x2 && equal_ident x1' x2' && equal_clause c1 c2 then
        Some (x1, c1, x1')
      else
        raise @@ MergeFailure(key, v1, v2)
  ;;

  let _merge_two_abort_points key (a : abort_value option) (b : abort_value option) =
    match a, b with
    | None, None -> None
    | Some x, None -> Some x
    | None, Some x -> Some x
    | Some (x1), Some (x2) ->
      if equal_abort_value x1 x2 then
        Some x1
      else
        raise @@ MergeAbortsFailure (key, x1, x2)
  ;;

  let merge_logs (log1 : log) (log2 : log) : log option =
    let open Option.Monad in
    let%bind merged_solver =
      try
        Some(Solver.union log1.log_solver log2.log_solver)
      with
        Solver.Contradiction contradiction ->
          _trace_log_solver_contradiction log1 log2 contradiction;
        None
    in
    let%bind merged_decisions =
      try
        let d1 = log1.log_decisions in
        let d2 = log2.log_decisions in
        Some(Relative_stack.Map.merge _merge_two_decisions d1 d2)
      with
        MergeFailure(key, v1, v2) ->
          _trace_log_decision_contradiction key v1 v2;
        None
    in
    let%bind merged_abort_points =
      try
        let ap1 = log1.log_abort_points in
        let ap2 = log2.log_abort_points in
        Some(Symbol_map.merge _merge_two_abort_points ap1 ap2)
      with
        MergeAbortsFailure(key, v1, v2) ->
          lazy_logger `trace (fun () ->
            Printf.sprintf
              "Failure to merge abort point %s with abort info \n%s\n and\n%s"
              (show_symbol key)
              (show_abort_value v1)
              (show_abort_value v2)
          );
        None
    in
    let new_log =
      { log_solver = merged_solver;
        log_decisions = merged_decisions;
        log_abort_points = merged_abort_points;
        log_steps = log1.log_steps + log2.log_steps;
      }
    in
    return new_log
  ;;

  (* **** Monadic operations **** *)

  (* ret function *)
  let return (v : 'a) : 'a m =
    M(fun state -> ([Unblocked(Completed (v, empty_log))], state))
  ;;

  let zero () : 'a m = M(fun state -> ([], state));;

  (* bind function and helpers *)

  (* Merge the log associated with a computation with a new log *)
  let rec _append_log (log : log) (x : 'b blockable) : 'b blockable option =
    match x with
    | Unblocked(Completed(value, log')) ->
      (* Directly merge the two logs - KQ *)
      begin
        match merge_logs log' log with
        | None -> None
        | Some log'' -> Some(Unblocked(Completed(value, log'')))
      end
    | Unblocked(Suspended(m, log')) ->
      (* Run the suspended computation once before merging logs - KQ *)
      begin
        let M(consumer_fn) = m in
        let consumer_fn' state =
          let results, state' = consumer_fn state in
          let results' = List.filter_map (_append_log log) results in
          results', state'
        in
        let m' = M(consumer_fn') in
        match merge_logs log' log with
        | None -> None
        | Some log'' -> Some(Unblocked(Suspended(m', log'')))
      end
    | Blocked(blocked) ->
      (* Merge the logs, then run it through the blocked consumer - KQ *)
      begin
        let consumer_fn' (result, log') =
          match merge_logs log' log with
          | None -> zero ()
          | Some log'' -> blocked.blocked_consumer (result, log'')
        in
        Some(Blocked({blocked with blocked_consumer = consumer_fn'}))
      end

  and _bind_single_world
      (f : 'a -> 'b m)
      (accumulator : 'b blockable list list * state)
      (world : 'a blockable)
    : 'b blockable list list * state =
    let (result_worlds, fold_state) = accumulator in
    match world with
    | Unblocked(Completed(value, log)) ->
      let M(consumer_fn) = f value in
      let results, fold_state' = consumer_fn fold_state in
      let results' = List.filter_map (_append_log log) results in
      (results' :: result_worlds, fold_state')
    | Unblocked(Suspended(m, log)) ->
      let M(consumer_fn) = m in
      let m' = M(_bind_worlds_fn f consumer_fn) in
      ([Unblocked(Suspended(m', log))] :: result_worlds, fold_state)
    | Blocked(blocked) ->
      let consumer_fn' (result, log') =
        let M(inner_world_fn) = blocked.blocked_consumer (result, log') in
        M(_bind_worlds_fn f inner_world_fn)
      in
      let blocked' = {blocked with blocked_consumer = consumer_fn'} in
      ([Blocked(blocked')] :: result_worlds, fold_state)

  and _bind_worlds_fn
      (f : 'a -> 'b m)
      (worlds_fn : state -> 'a blockable list * state)
    : state -> 'b blockable list * state =
    fun state -> (
      let (worlds, state') = worlds_fn state in
      let bound_worlds, state'' =
        List.fold_left (_bind_single_world f) ([], state') worlds
      in
      (List.concat bound_worlds, state''))

  and bind (x: 'a m) (f: 'a -> 'b m) : 'b m =
    let M(worlds_fn) = x in
    M(_bind_worlds_fn f worlds_fn)
  ;;

  let pick (items : 'a Enum.t) : 'a m =
    let computation_list =
      items
      |> Enum.map (fun x -> Unblocked(Completed(x, empty_log)))
      |> List.of_enum
    in
    M(fun state -> computation_list, state)
  ;;

  let pause () : unit m =
    let single_step_log = { empty_log with log_steps = 1 } in
    let completed_val = Unblocked(Completed((), single_step_log)) in
    let completed_mon = M(fun state -> ([completed_val], state)) in
    let suspended_val = Unblocked(Suspended(completed_mon, empty_log)) in
    let suspended_mon = M(fun state -> ([suspended_val], state)) in
    suspended_mon
  ;;

  (* Wrap a log in a monadic value with an empty computation *)
  let _record_log (log : log) : unit m =
    M(fun state -> [Unblocked(Completed((), log))], state)
  ;;

  let cache (key : 'a Cache_key.t) (value : 'a m) : 'a m =
    let new_consumer =
      (fun (item, log) ->
        let%bind () = _record_log log in
        return item)
    in
    let blocked =
        { blocked_key = key;
          blocked_consumer = new_consumer;
          blocked_computation = value;
        }
    in
    M(fun state -> ([Blocked(blocked)], state))
  ;;

  let cache_abort (key : 'a Cache_key.t) (value : 'a m) : 'a m =
    let new_consumer =
      (fun (_, log) ->
        (lazy_logger `debug (fun () ->
          Printf.sprintf
            (* TODO: fix up formatting *)
            ("Abort lookup successful at %s\n" ^^
             "Formulae:\n%s\n" ^^
             "Decisions:\n%s\n")
            (Cache_key.show key)
            (Solver.show log.log_solver)
            (Relative_stack.Map.show pp_decision log.log_decisions)
        ));
        zero ();
      )
    in
    let blocked =
        { blocked_key = key;
          blocked_consumer = new_consumer;
          blocked_computation = value;
        }
    in
    M(fun state -> ([Blocked(blocked)], state))
  ;;

  let record_decision
      (s : Relative_stack.t) (x : Ident.t) (c : clause) (x' : Ident.t)
    : unit m =
    _record_log @@
    { log_solver = Solver.empty;
      log_decisions = Relative_stack.Map.singleton s (x,c,x');
      log_abort_points = Symbol_map.empty;
      log_steps = 0;
    }
  ;;

  let record_constraint (c : Constraint.t) : unit m =
    _record_log @@
    { log_solver = Solver.singleton c;
      log_decisions = Relative_stack.Map.empty;
      log_abort_points = Symbol_map.empty;
      log_steps = 0;
    }
  ;;

  let record_abort_point
      (ab_symbol: symbol) (ab_info: abort_value)
    : unit m =
    _record_log @@
    { log_solver = Solver.empty;
      log_decisions = Relative_stack.Map.empty;
      log_abort_points = Symbol_map.singleton ab_symbol ab_info;
      log_steps = 0;
    }
  ;;

  (* check_constraint function + helpers *)

  let _trace_log_contradiction (log : log) : state =
    lazy_logger `trace @@ fun () ->
      Printf.sprintf
        "SAT contradiction at formulae check in:\n%s\n"
        (Solver.show log.log_solver)
  ;;

  (* Returns a function that zeros out a particular world if it has a
     contradiction *)
  let rec check_one_world : 'a. 'a blockable -> 'a blockable option =
    fun blockable ->
      match blockable with
      | Unblocked(Completed(_, log)) ->
        (* Check if the completed computation is satisfiable or not *)
        if Solver.solvable log.log_solver then
          Some(blockable)
        else begin
          _trace_log_contradiction log;
          None
        end
      | Unblocked(Suspended(m, log)) ->
        (* Check the computations within the monadic value *)
        Some(Unblocked(Suspended(check_constraints m, log)))
      | Blocked(blocked) ->
        (* Check the computation that is being blocked on *)
        let new_blocked =
          { blocked with
            blocked_computation = check_constraints blocked.blocked_computation
          }
        in
        Some(Blocked(new_blocked))

  (* Returns a function that will zero out any worlds with contradictions *)
  and check_constraints : 'a. 'a m -> 'a m =
    fun x ->
      let M(worlds_fn) = x in
      let consumer_fn state =
        let (blockables, state') = worlds_fn state in
        let blockables' = List.filter_map check_one_world blockables in
        (blockables', state')
      in
      M(consumer_fn)
  ;;

  (** ****************** BEGIN EVALUATION SUBMODULE ******************** *)

  type 'out evaluation_result =
    { er_value : 'out;
      er_solver : Solver.t;
      er_abort_points : abort_value Symbol_map.t;
      er_evaluation_steps : int;
      er_result_steps : int;
    };;

  module type Type = sig type t;; end;;

  module type Evaluation_sig = sig
    type out;;
    type evaluation;;
    val start : out m -> evaluation;;
    val step :
      ?show_value:(out -> string) -> evaluation ->
      (out evaluation_result Enum.t * evaluation);;
    val is_complete : evaluation -> bool;;
  end;;

  (** The evaluation process is primarily defined within a functor to permit
      the output type of the monadic value being evaluated to be fixed.  This
      functor is invoked dynamically and the black block evaluation state given
      to the user contains a reference to the appropriate first-class module. *)
  module Evaluation(Out : Type) : Evaluation_sig with type out = Out.t = struct
    (* **** Evaluation types **** *)

    (** Output type *)
    type out = Out.t;;

    (** A task is a computation together with the place to which its
        value should be sent upon completion.  The Cache_task constructor
        specifies a cache destination (to resume a blocked computation).  The
        Result_task constructor's place is implicitly the output of the overall
        computation. *)
    type _ task =
      | Cache_task : 'a Cache_key.t * 'a m -> 'a task
      | Result_task : out m -> out task
    ;;

    type some_task = Some_task : 'a task -> some_task;;

    let string_of_some_task (Some_task(task)) =
      match task with
      | Cache_task(key,_) -> "Task for destination " ^ Cache_key.show key
      | Result_task _ -> "Task for result"
    ;;

    type 'a consumer = Consumer of ('a * log -> some_task);;

    type 'a destination =
      { dest_consumers : 'a consumer list;
        dest_values : ('a * log) list;
      }
    ;;

    module Key = struct
      type 'a t = K : 'a Cache_key.t -> 'a destination t;;
      let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t =
        fun k k' ->
          match k, k' with
          | K(ck), K(ck') ->
            begin
              match Cache_key.compare ck ck' with
              | Lt -> Lt
              | Eq -> Eq
              | Gt -> Gt
            end
      ;;
    end;;

    module Destination_map = Gmap.Make(Key);;

    type evaluation =
      { ev_state : state;
        ev_tasks : some_task Work_collection.t;
        ev_destinations : Destination_map.t;
        ev_evaluation_steps : int;
      }
    ;;

    type 'a some_blocked = Some_blocked : ('z,'a) blocked -> 'a some_blocked;;

    (* **** Console logging operations **** *)

    let _trace_log_evaluation_state (ev : evaluation) : unit =
      lazy_logger `trace
        (fun () ->
           Printf.sprintf
            ("Work collection has %d items:\n%s\n" ^^
             "Messaging state:\n%s\n")
            (Work_collection.size ev.ev_tasks)
            (Work_collection.enum ev.ev_tasks
              |> Enum.map string_of_some_task
              |> Enum.map (fun s -> "* " ^ s)
              |> List.of_enum
              |> String.join "\n")
            (ev.ev_destinations
              |> Destination_map.bindings
              |> List.map
                (fun (Destination_map.B(K(key), value)) ->
                  let key_str = Cache_key.show key in
                  let consumer_count = List.length value.dest_consumers in
                  let value_count = List.length value.dest_values in
                  Printf.sprintf "* %s --> %d values, %d consumers"
                    key_str value_count consumer_count
                )
              |> String.join "\n")
        );
    ;;

    let _trace_log_start_step ev =
      lazy_logger `trace (fun () -> "Stepping monadic evaluation.");
      _trace_log_evaluation_state ev;
    ;;

    let _trace_log_end_step ev =
      lazy_logger `trace (fun () -> "Finished stepping monadic evaluation.");
      _trace_log_evaluation_state ev;
    ;;

    let _trace_log_describe_task (task : some_task) : state =
      let task_description =
        match task with
        | Some_task(Cache_task(key, _)) ->
          "cache key " ^ Cache_key.show key
        | Some_task(Result_task _) ->
          "result"
      in
      lazy_logger `trace (fun () -> "Stepping task for " ^ task_description)
    ;;

    let _trace_log_cache_miss blocked =
      lazy_logger `trace
        (fun () -> "Cache miss for key " ^ Cache_key.show blocked.blocked_key)
    ;;

    let _trace_log_cache_hit blocked =
      lazy_logger `trace
        (fun () -> "Cache hit for key " ^ Cache_key.show blocked.blocked_key)
    ;;

    let _debug_log_step_output
        ?show_value:(show_value=fun _ -> "<no printer>")
        (final_ev : evaluation)
        (value : out)
        (log : log)
      : state =
      lazy_logger `debug (fun () ->
        Printf.sprintf
          ("Symbolic monad evaluation produced a result:\n" ^^
           "  Value:\n    %s\n" ^^
           "  Formulae:\n    %s\n" ^^
           "  Decisions:\n    %s\n" ^^
           "  Abort program points:\n    %s\n" ^^
           "  Result steps: %d\n" ^^
           "  Cache size: %d\n")
          (show_value value)
          (Solver.show log.log_solver)
          (Relative_stack.Map.show pp_decision log.log_decisions)
          (Symbol_map.show pp_abort_value log.log_abort_points)
          (log.log_steps + 1)
          (Destination_map.cardinal final_ev.ev_destinations)
      )
    ;;

    (* **** Evaluation operations **** *)

    (* Adding tasks (used in both start and step) *)

    (** Adds a task to an evaluation's queue. *)
    let _add_task (task : some_task) (ev : evaluation) : evaluation =
      let cache_key_opt =
        match task with
        | Some_task(Result_task _) -> None
        | Some_task(Cache_task(key,_)) -> Some(Cache_key.Some_key key)
      in
      let item =
        { work_item = task;
          work_cache_key = cache_key_opt;
        }
      in
      { ev with ev_tasks = Work_collection.offer item ev.ev_tasks }
    ;;

    (** Adds many tasks to an evaluation's queue. *)
    let _add_tasks (tasks : some_task Enum.t) (ev : evaluation) : evaluation =
      Enum.fold (flip _add_task) ev tasks
    ;;

    (* Start *)
  
    (** Initializes a computation in this monad, similar to "run". *)
    let start (x : out m) : evaluation =
      let initial_state = () in
      let empty_evaluation =
        { ev_state = initial_state;
          ev_tasks = Work_collection.empty;
          ev_destinations = Destination_map.empty;
          ev_evaluation_steps = 0;
        }
      in
      _add_task (Some_task(Result_task x)) empty_evaluation
    ;;

    (* **** Step + helper functions **** *)

    (** For a particular world, determine if it is a completed result, a
        suspended computation, or a blocking computation. - KQ *)
    let _sort_world accumulator world =
      let (complete, suspended, blocked) = accumulator in
      match world with
      | Unblocked(Completed(value, log)) ->
        ((value, log) :: complete, suspended, blocked)
      | Unblocked(Suspended(m, _)) ->
        (complete, m :: suspended, blocked)
      | Blocked(bl) ->
        (complete, suspended, (Some_blocked(bl)) :: blocked)
    ;;
  
    (** Step into the monadic value to extract the results of a computation.
        - KQ *)
    let _step_m (state : state) (x : 'a m) :
      ('a * log) Enum.t *     (* Completed results *)
      'a m list *             (* Suspended computations *)
      'a some_blocked list *  (* Blocking computations *)
      state                   (* Resulting state *)
      =
      (* Unfreeze the function in the monadic value *)
      let M(world_fn) = x in
      let (worlds, state') = world_fn state in
      (* Sort each world into one of three categories *)
      let (complete, suspended, blocked) =
        List.fold_left _sort_world ([],[],[]) worlds
      in
      (List.enum complete, suspended, blocked, state')
    ;;

    (** Add a new cache task to the cache, and add a new destination to the
        key. - KQ *)
    let _create_cache_entry (ev : evaluation) (blocked : ('a, 'b) blocked)
      : evaluation =
      let key = blocked.blocked_key in
      let computation = blocked.blocked_computation in
      let dest = { dest_consumers = []; dest_values = []; } in
      let task = Some_task(Cache_task(key, computation)) in
      let destination_map' =
        Destination_map.add (K(key)) dest ev.ev_destinations
      in
      let ev' = { ev with ev_destinations = destination_map'; } in
      let ev'' = _add_task task ev' in
      ev''
    ;;

    (** Registers a consumer to a cache destination.  This adds the consumer to
        the destination and processes the "catch-up" of the consumer on all
        previously produced values. *)
    let _register_consumer
        (key : 'a Cache_key.t)
        (consumer : 'a consumer)
        (ev : evaluation)
      : evaluation =
      match Destination_map.find (K(key)) ev.ev_destinations with
      | None ->
        (* This should never happen:
           1. _register_consumer is only called when a blocked computation is
              discovered
           2. That code immediately adds a destination to the evaluation
              environment under this key
        *)
        raise @@ Utils.Invariant_failure
          "Destination not established by the time it was produced!"
      | Some destination ->
        (* Start by registering the consumer. *)
        let destination' =
          { destination with
            dest_consumers = consumer :: destination.dest_consumers;
          }
        in
        let destination_map' =
          Destination_map.add (K(key)) destination' ev.ev_destinations
        in
        let ev' = { ev with ev_destinations = destination_map' } in
        (* Now catch the consumer up on all of the previous values. *)
        let Consumer consumer_fn = consumer in
        let new_tasks =
          destination.dest_values
          |> List.enum
          |> Enum.map consumer_fn
        in
        (* Add the new tasks to the evaluation environment *)
        _add_tasks new_tasks ev'
    ;;

    (** Add a blocked computation to the evaluation environment, as a
        consumer. If that computation is currently not in the cache,
        we first need to add it. - KQ *)
    let _add_blocked_to_evaluation
        (mk_task : 't m -> some_task)
        (ev : evaluation)
        (some_blocked : 't some_blocked)
      : evaluation =
      let Some_blocked(blocked) = some_blocked in
      let key = blocked.blocked_key in
      let ev' =
        (* Ensure that the destination for this computation exists. *)
        match Destination_map.find (K(key)) ev.ev_destinations with
        | None ->
          (* We've never heard of this destination before.  That means
             this is the first cache request on this key, so we should
             create it and then start computing for it. *)
          _trace_log_cache_miss blocked;
          _create_cache_entry ev blocked
        | Some _ ->
          (* This destination already exists.  We don't need to do
             anything to make it ready for the consumer to be
             registered. *)
          _trace_log_cache_hit blocked;
          ev
      in
      (* Create the new consumer from the blocked evaluation.  Once it
         receives a value, the blocked evaluation will produce a new
         computation intended for the same destination, just as with a
         suspended computation. *)
      let new_consumer_fn =
        fun (value,log) ->
          let computation = blocked.blocked_consumer (value, log) in
          mk_task computation
      in
      let new_consumer = Consumer(new_consumer_fn) in
      let ev'' = _register_consumer key new_consumer ev' in
      ev''
    ;;

    (** Processes the production of a value at a cache destination.  This adds
        the value to the destination and calls any consumers listening to it *)
    let _produce_value_at_dest
        (key : 'a Cache_key.t)
        (value : 'a)
        (log : log)
        (ev : evaluation)
      : evaluation =
      match Destination_map.find (K(key)) ev.ev_destinations with
      | None ->
        (* This should never happen:
           1. _produce_value_at_dest is only called when a cache task produces
              a value
           2. A cache task can only produce a value if it has been started
           3. Cache tasks are only started by blocked computations
           4. Blocked computations create a destination for their keys
        *)
        raise @@ Utils.Invariant_failure
          "Destination not established by the time it was produced!"
      | Some destination ->
        (* Start by adding the new value. *)
        let destination' =
          { destination with
            dest_values = (value, log) :: destination.dest_values;
          }
        in
        let destination_map' =
          Destination_map.add (K(key)) destination' ev.ev_destinations
        in
        let ev' = { ev with ev_destinations = destination_map' } in
        (* Now create the tasks from the consumers. *)
        let new_tasks =
          destination.dest_consumers
          |> List.enum
          |> Enum.map
            (fun consumer ->
               let Consumer consumer_fn = consumer in
               consumer_fn (value, log)
            )
        in
        (* Add the tasks to the evaluation environment. *)
        _add_tasks new_tasks ev'
    ;;

    (** Process the output to make it presentable to the caller. *)
    
    (* The +1 for er_result_steps is to ensure that the number of steps
        reported is equal to the number of times the "step" function has been
        called.  For a given result, the "step" function must be called once
        for each "pause" (which is handled inductively when the "pause" monadic
        function modifies the log) plus once because the "start" routine
        implicitly pauses computation. This +1 addresses what the "start"
        routine does. *)
    let _process_output
        ?show_value:(show_value=fun _ -> "<no printer>")
        (final_ev : evaluation)
        (completed : (out * log) Enum.t)
      : out evaluation_result Enum.t =
      completed
      |> Enum.map
        (fun (value, log) ->
            (*
            let match_symb_list =
              log.log_abort_points
              |> Symbol_map.values
              |> List.of_enum
              |> List.flatten
            in
            let type_errors =
              let find_err_fn = Solver.find_type_error log.log_solver in
              List.filter_map find_err_fn match_symb_list
            in
            *)
            _debug_log_step_output ~show_value:show_value final_ev value log;
            { er_value = value;
              er_solver = log.log_solver;
              (* er_type_errors = type_errors; *)
              er_abort_points = log.log_abort_points;
              er_evaluation_steps = final_ev.ev_evaluation_steps;
              er_result_steps = log.log_steps + 1;
            }
        )
    ;;

    (* A destination in the destination map should have at least one consumer,
       starting from the consumer in _add_blocked_to_evaluation. If a
       destination has no consumers after a step, then something has gone
       horribly wrong. - KQ *)
    let _step_sanity_check ev =
      ev.ev_destinations
      |> Destination_map.bindings
      |> List.iter
        (fun (Destination_map.B(K(key), value)) ->
            if List.is_empty value.dest_consumers then
              failwith @@
                Printf.sprintf "ERROR! Cache key missing destinations: %s"
                (Cache_key.show key)
        )
    ;;

    (** Do the work on a task and get the completed results; the suspended
        computations will be added as future tasks and the blocked computations
        will be added as consumers. - KQ *)
    let handle_computation
        (mk_task : 't m -> some_task)
        (computation : 't m)
        (ev : evaluation)
        (remaining_tasks : some_task Work_collection.t)
      : (('t * log) Enum.t * evaluation) =
      (* Evaluation with the taken task removed *)
      let ev' = {ev with ev_tasks = remaining_tasks} in
      (* Do the work that is asked. (Step 1) *)
      let (complete, suspended, blocked, state') =
        _step_m ev.ev_state computation
      in
      (* Update our evaluation to reflect the state change and the step. *)
      let ev_after_step =
        { ev_state = state';
          ev_tasks = ev'.ev_tasks;
          ev_destinations = ev'.ev_destinations;
          ev_evaluation_steps = ev'.ev_evaluation_steps + 1;
        }
      in
      (* From the completed task, any suspended computations get added to the
         task list with the same destination. (Step 4) *)
      let ev_after_processing_suspended =
        suspended
        |> List.enum
        |> Enum.map mk_task
        |> flip _add_tasks ev_after_step
      in
      (* From the completed task, all blocked computations should be added as
         consumers of the cache key on which they are blocking.  If the cache
         key has not been seen previously, then its computation should be
         started. (Step 5) *)
      let ev_after_processing_blocked =
        blocked
        |> List.enum
        |> Enum.fold
          (_add_blocked_to_evaluation mk_task) ev_after_processing_suspended
      in
      (complete, ev_after_processing_blocked)
    ;;

    (** Performs one step of evaluation, until all outcomes are paused. *)

    (* TODO: Shouldn't the order be 1, 4, 5, 2, 3? *)
    (* The overall strategy of the algorithm below is:
         1. Get a task and do the work.
         2. If the task is for a cached result, dispatch any new values to
            the registered consumers.
         3. If the task is for a final result, communicate any new values to
              the caller.
         4. Add all suspended computations as future tasks.
         5. Add all blocked computations as consumers.
       Because of how type variables are scoped, however, steps 1, 2, and 3
       must be within the respective match branches which destructed the task
       values.  We'll push what we can into local functions to limit code
       duplication.  In particular, steps 1, 4, and 5 are performed in the
       handle_computation function, since they are performed for both cache
       and result tasks, while steps 2 and 3 are done in this function. *)
    let step ?show_value:(show_value=fun _ -> "<no printer>") (ev : evaluation)
      : (out evaluation_result Enum.t * evaluation) =
      (* Start stepping *)
      _trace_log_start_step ev;
      (* Get a single task from the work collection *)
      match Work_collection.take ev.ev_tasks with
      | None ->
        (* There is no work left to do.  Don't step. *)
        (Enum.empty(), ev)
      | Some({work_item = task; work_cache_key = _}, tasks') ->
        _trace_log_describe_task task;
        let ((completed : (out * log) Enum.t), final_ev) =
          match task with
          | Some_task(Cache_task(key, computation)) ->
            (* Do computation and update state. (Steps 1, 4, and 5) *)
            let mk_task =
              (fun computation -> Some_task(Cache_task(key, computation)))
            in
            let (complete, ev_after_computation) =
              handle_computation mk_task computation ev tasks'
            in
            (* Every value in the completed sequence should be reported to the
                destination specified by this key. (Step 2) *)
            let _report_value ev'' (value, log) =
              _produce_value_at_dest key value log ev''
            in
            let ev_after_completed =
              Enum.fold _report_value ev_after_computation complete
            in
            (Enum.empty(), ev_after_completed)
          | Some_task(Result_task(computation)) ->
            (* Do computation and update state. (Steps 1, 4, and 5) *)
            let mk_task =
              (fun computation -> Some_task(Result_task(computation)))
            in
            let (complete, ev_after_computation) =
              handle_computation mk_task computation ev tasks'
            in
            (* Every value in the completed sequence should be returned to the
                caller. (Step 3) *)
            (complete, ev_after_computation)
        in
        (* Process the output to make it presentable to the caller. *)
        let output =
          _process_output ~show_value:show_value final_ev completed
        in
        (* Finish stepping *)
        _trace_log_end_step final_ev;
        (* _step_sanity_check final_ev; *)
        (output, final_ev)
    ;;

    (** **** is_complete **** *)

    (** Determines whether a particular evaluation has completed. *)
    let is_complete (ev : evaluation) : bool =
      Work_collection.is_empty ev.ev_tasks
    ;;
  end
  ;;

  (** ****************** END EVALUATION SUBMODULE ******************** *)

  (** **** Evaluation module wrapper **** *)

  (**
     An evaluation is a monadic value for which evaluation has started.  It is
     representative of all of the concurrent, non-deterministic computations
     being performed as well as all metadata (such as caching).
  *)
  type 'out evaluation =
      Evaluation :
        (module Evaluation_sig
          with type out = 'out
          and type evaluation = 'e) * 'e -> 'out evaluation
  ;;

  let start (type a) (x : a m) : a evaluation =
    let module E = Evaluation(struct type t = a end) in
    Evaluation((module E), E.start x)
  ;;

  let step
      (type a)
      ?show_value:(show_value=fun _ -> "<no printer>")
      (ev : a evaluation)
    : (a evaluation_result Enum.t * a evaluation) =
    let Evaluation(md, e) = ev in
    let (module E) = md in
    let (answers, e') = E.step ~show_value:show_value e in
    (answers, Evaluation(md, e'))
  ;;

  let is_complete (type a) (ev : a evaluation) : bool =
    let Evaluation(m,e) = ev in
    let (module E) = m in
    E.is_complete e
  ;;

end
;;
