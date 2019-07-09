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
open Sat_types;;

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
  val record_decision :
    Relative_stack.t -> Ident.t -> clause -> Ident.t -> unit m;;
  val record_formula : Formula.t -> unit m;;
  val check_formulae : 'a m -> 'a m;;

  type 'a evaluation;;

  type 'a evaluation_result =
    { er_value : 'a;
      er_formulae : Formulae.t;
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

  type decision_map =
    (Ident.t * clause * Ident.t) Relative_stack.Map.t
  [@@deriving show]
  ;;
  let _ = show_decision_map;;

  type log = {
    log_formulae : Formulae.t;
    log_decisions : decision_map;
    log_steps : int;
  } [@@deriving show];;
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

  let empty_log = {
    log_formulae = Formulae.empty;
    log_decisions = Relative_stack.Map.empty;
    log_steps = 0;
  };;

  exception MergeFailure of
      Relative_stack.t * (ident * clause * ident) * (ident * clause * ident);;

  let merge_logs (log1 : log) (log2 : log) : log option =
    let open Option.Monad in
    let%bind merged_formulae =
      try
        Some(Formulae.union log1.log_formulae log2.log_formulae)
      with
      | Formulae.SymbolTypeContradiction(_,symbol,types) ->
        (lazy_logger `trace @@ fun () ->
         Printf.sprintf
           "Immediate contradiction at symbol %s with types %s while merging two formula sets.\nSet 1:\n%s\nSet 2:\n%s\n"
           (show_symbol symbol)
           (Jhupllib.Pp_utils.pp_to_string (Jhupllib.Pp_utils.pp_list Formulae.pp_symbol_type) types)
           (Formulae.show_brief log1.log_formulae)
           (Formulae.show_brief log2.log_formulae)
        );
        None
      | Formulae.SymbolValueContradiction(_,symbol,v1,v2) ->
        (lazy_logger `trace @@ fun () ->
         Printf.sprintf
           "Immediate contradiction at symbol %s with values %s and %s while merging two formula sets.\nSet 1:\n%s\nSet 2:\n%s\n"
           (show_symbol symbol)
           (show_value v1) (show_value v2)
           (Formulae.show_brief log1.log_formulae)
           (Formulae.show_brief log2.log_formulae)
        );
        None
    in
    let merge_fn key a b =
      match a,b with
      | None,None -> None
      | Some x,None -> Some x
      | None,Some x -> Some x
      | Some((x1,c1,x1') as v1),Some((x2,c2,x2') as v2) ->
        if equal_ident x1 x2 && equal_ident x1' x2' && equal_clause c1 c2 then
          Some(x1,c1,x1')
        else
          raise @@ MergeFailure(key, v1, v2)
    in
    let%bind merged_decisions =
      try
        Some(Relative_stack.Map.merge
               merge_fn log1.log_decisions log2.log_decisions)
      with
      | MergeFailure(key, v1, v2) ->
        begin
          let show_v =
            Pp_utils.pp_to_string
              (Pp_utils.pp_triple pp_ident Ast_pp_brief.pp_clause pp_ident)
          in
          lazy_logger `trace (fun () ->
              Printf.sprintf
                "Contradiction while merging %s decisions: %s and %s"
                (Relative_stack.show key) (show_v v1) (show_v v2)
            );
          None
        end
    in
    let new_log =
      { log_formulae = merged_formulae;
        log_decisions = merged_decisions;
        log_steps = log1.log_steps + log2.log_steps;
      }
    in
    return new_log
  ;;

  (* **** Monadic operations **** *)

  let return (type a) (v : a) : a m =
    M(fun cache -> ([Unblocked(Completed (v, empty_log))], cache))
  ;;

  let zero (type a) () : a m = M(fun state -> ([], state));;

  let _record_log (log : log) : unit m =
    M(fun state -> [Unblocked(Completed((), log))], state)
  ;;

  let bind (type a) (type b) (x : a m) (f : a -> b m) : b m =
    let rec append_log (log : log) (x : b blockable) : b blockable option =
      match x with
      | Unblocked(Completed(value,log')) ->
        begin
          match merge_logs log' log with
          | None -> None
          | Some log'' -> Some(Unblocked(Completed(value,log'')))
        end
      | Unblocked(Suspended(m,log')) ->
        let M(fn) = m in
        let fn' cache =
          let results,cache' = fn cache in
          let results' = List.filter_map (append_log log) results in
          results',cache'
        in
        let m' = M(fn') in
        begin
          match merge_logs log' log with
          | None -> None
          | Some log'' -> Some(Unblocked(Suspended(m',log'')))
        end
      | Blocked(blocked) ->
        let fn' (result,log') =
          match merge_logs log' log with
          | None -> zero ()
          | Some log'' -> blocked.blocked_consumer (result, log'')
        in
        Some(Blocked({blocked with blocked_consumer = fn'}))
    in
    let rec bind_worlds_fn
        (worlds_fn : state -> a blockable list * state) (state : state)
      : b blockable list * state =
      let worlds, state' = worlds_fn state in
      let bound_worlds, state'' =
        worlds
        |> List.fold_left
          (fun (result_worlds, fold_state) world ->
             match world with
             | Unblocked(Completed(value,log)) ->
               let M(fn) = f value in
               let results, fold_cache' = fn fold_state in
               let results' = List.filter_map (append_log log) results in
               (results'::result_worlds, fold_cache')
             | Unblocked(Suspended(m,log)) ->
               let M(fn) = m in
               let m' = M(bind_worlds_fn fn) in
               ([Unblocked(Suspended(m',log))]::result_worlds, fold_state)
             | Blocked(blocked) ->
               let fn' (result,log') =
                 let M(inner_world_fn) =
                   blocked.blocked_consumer (result,log')
                 in
                 (* Here, the monadic value is the result of passing a cached
                    result to the previous caching function.  Once we have
                    that information, we can do the bind against that monadic
                    value. *)
                 M(bind_worlds_fn inner_world_fn)
               in
               let blocked' =
                 { blocked_key = blocked.blocked_key;
                   blocked_consumer = fn';
                   blocked_computation = blocked.blocked_computation;
                 }
               in
               ([Blocked(blocked')]::result_worlds, fold_state)
          )
          ([], state')
      in
      (List.concat bound_worlds, state'')
    in
    let M(worlds_fn) = x in
    M(bind_worlds_fn worlds_fn)
  ;;

  let pick (type a) (items : a Enum.t) : a m =
    M(fun state ->
       (items
        |> Enum.map (fun x -> Unblocked(Completed(x, empty_log)))
        |> List.of_enum
       ),
       state
     )
  ;;

  let pause () : unit m =
    M(fun state ->
       let single_step_log = {empty_log with log_steps = 1} in
       let completed_value = Unblocked(Completed((), single_step_log)) in
       let suspended_value =
         Suspended(M(fun state -> ([completed_value], state)), empty_log)
       in
       ([Unblocked(suspended_value)], state)
     )
  ;;

  let cache (key : 'a Cache_key.t) (value : 'a m) : 'a m =
    M(fun state ->
       let blocked =
         { blocked_key = key;
           blocked_consumer =
             (fun (item,log) ->
                let%bind () = _record_log log in
                return item);
           blocked_computation = value;
         }
       in
       ([Blocked(blocked)], state)
     )
  ;;

  let record_decision
      (s : Relative_stack.t) (x : Ident.t) (c : clause) (x' : Ident.t)
    : unit m =
    _record_log @@
    { log_formulae = Formulae.empty;
      log_decisions = Relative_stack.Map.singleton s (x,c,x');
      log_steps = 0;
    }
  ;;

  let record_formula (formula : Formula.t) : unit m =
    _record_log @@
    { log_formulae = Formulae.singleton formula;
      log_decisions = Relative_stack.Map.empty;
      log_steps = 0;
    }
  ;;

  let rec check_formulae : 'a. 'a m -> 'a m =
    fun x ->
      let check_one_world : 'a. 'a blockable -> 'a blockable option =
        fun blockable ->
          match blockable with
          | Unblocked(Completed(_,log)) ->
            if Solver.solvable log.log_formulae then
              Some(blockable)
            else begin
              (lazy_logger `trace @@ fun () ->
               Printf.sprintf
                 "SAT contradiction at formulae check in:\n%s\n"
                 (Formulae.show_brief log.log_formulae)
              );
              None
            end
          | Unblocked(Suspended(m,log)) ->
            Some(Unblocked(Suspended(check_formulae m, log)))
          | Blocked(blocked) ->
            Some(Blocked(
                { blocked with
                  blocked_computation =
                    check_formulae blocked.blocked_computation
                }))
      in
      let M(worlds_fn) = x in
      let fn state =
        let (blockables, state') = worlds_fn state in
        let blockables' = List.filter_map check_one_world blockables in
        (blockables', state')
      in
      M(fn)
  ;;

  (* **** Evaluation module **** *)

  type 'out evaluation_result =
    { er_value : 'out;
      er_formulae : Formulae.t;
      er_evaluation_steps : int;
      er_result_steps : int;
    };;


  module Key = struct
    type 'a t = K : 'a Cache_key.t -> ('a * log) t;;
    let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun k k' ->
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

  module Cache_map = Gmap.Make(Key);;

  type 'out task =
    | Cache_task : 'a Cache_key.t * 'a m * ('a * log -> 'out task) -> 'out task
    | Result_task : 'out m -> 'out task
  ;;

  type 'out search_state =
    { ss_cache : Cache_map.t;
      ss_task : 'out task;
    }
  ;;

  type 'out evaluation =
    { ev_state : state;
      ev_work : 'out search_state Work_collection.t;
      ev_evaluation_steps : int;
    }
  ;;

  type 'a some_blocked = Some_blocked : ('z,'a) blocked -> 'a some_blocked;;

  (* **** Evaluation operations **** *)

  let start (x : 'out m) : 'out evaluation =
    let task = Result_task x in
    let state =
      { ss_cache = Cache_map.empty;
        ss_task = task;
      }
    in
    let work_info =
      { work_item = state;
        work_cache_key = None;
      }
    in
    let collection = Work_collection.offer work_info Work_collection.empty in
    let initial_state = () in
    { ev_state = initial_state;
      ev_work = collection;
      ev_evaluation_steps = 0;
    }
  ;;

  let _step_m (type a) (state : state) (x : a m)
    : (a * log) Enum.t * a m Enum.t * a some_blocked Enum.t * state =
    let M(world_fn) = x in
    let (worlds,state') = world_fn state in
    let completed, suspended, blocked =
      worlds
      |> List.fold_left
        (fun (completed, suspended, blocked) world ->
           match world with
           | Unblocked(Completed(value, log)) ->
             ((value, log) :: completed, suspended, blocked)
           | Unblocked(Suspended(x', _)) ->
             (completed, x' :: suspended, blocked)
           | Blocked(blocked_item) ->
             (completed, suspended, (Some_blocked blocked_item) :: blocked)
        )
        ([], [], [])
    in
    (List.enum completed, List.enum suspended, List.enum blocked, state')
  ;;

  let suspended_and_blocked_states
      (type out) (type a)
      (mk_task : a m -> out task)
      (cache : Cache_map.t)
      (suspended : a m Enum.t) (blocked : a some_blocked Enum.t)
    : out task Enum.t =
    let suspended_states =
      suspended
      |> Enum.map (fun m -> mk_task m)
    in
    let blocked_states =
      blocked
      |> Enum.map
        (fun (Some_blocked(blocked)) ->
           match Cache_map.find (K(blocked.blocked_key)) cache with
           | None ->
             lazy_logger `trace
               (fun () ->
                  "Cache miss for key " ^ Cache_key.show blocked.blocked_key);
             let continuation (value, log) =
               let m = blocked.blocked_consumer (value, log) in
               mk_task m
             in
             Cache_task(
               blocked.blocked_key,
               blocked.blocked_computation,
               continuation
             )
           | Some value_and_log ->
             lazy_logger `trace
               (fun () ->
                  "Cache hit for key " ^ Cache_key.show blocked.blocked_key);
             mk_task @@ blocked.blocked_consumer value_and_log
        )
    in
    Enum.append suspended_states blocked_states
  ;;

  let _search_to_work_item search =
    { work_item = search;
      work_cache_key =
        (
          match search.ss_task with
          | Cache_task(key,_,_) -> Some(Cache_key.Some_key key)
          | Result_task _ -> None
        );
    }
  ;;

  let step
      (type out)
      ?show_value:(_show_value=fun _ -> "<no printer>")
      (ev : out evaluation)
    : (out evaluation_result Enum.t * out evaluation) =
    match Work_collection.take ev.ev_work with
    | None ->
      (* No further results exist to report. *)
      (Enum.empty (), ev)
    | Some(work, collection') ->
      let step_count = ev.ev_evaluation_steps + 1 in
      let search = work.work_item in
      lazy_logger `trace
        (fun () ->
           Printf.sprintf
             "Stepping search state for %s with cache size %d"
             (match search.ss_task with
              | Result_task _ -> "result"
              | Cache_task (key, _, _) -> Cache_key.show key
             )
             (Cache_map.cardinal search.ss_cache)
        );
      begin
        match search.ss_task with
        | Result_task monadic ->
          (* Step monadic value. *)
          let (completed, suspended, blocked, state') =
            _step_m ev.ev_state monadic
          in
          (* Process completed values by building evaluation results. *)
          let results : out evaluation_result Enum.t =
            completed
            |> Enum.map
              (fun (value, log) ->
                 { er_value = value;
                   er_formulae = log.log_formulae;
                   er_evaluation_steps = step_count;
                   er_result_steps = log.log_steps + 1;
                   (* The +1 here is to ensure that the number of steps reported
                      is equal to the number of times the "step" function has
                      been called.  For a given result, the "step" function must
                      be called once for each "pause" (which is handled
                      inductively when the "pause" monadic function modifies the
                      log) plus once because the "start" routine implicitly
                      pauses computation.  This +1 addresses what the "start"
                      routine does. *)
                 }
              )
          in
          (* Identify new tasks from suspended and blocked values. *)
          let new_work =
            suspended_and_blocked_states
              (fun m -> Result_task m) search.ss_cache suspended blocked
            |> Enum.map
              (fun task ->
                 { ss_task = task;
                   ss_cache = search.ss_cache;
                 }
              )
            |> Enum.map _search_to_work_item
          in
          (* Build new work collection. *)
          let collection'' =
            new_work
            |> Enum.fold (flip Work_collection.offer) collection'
          in
          (* Build new evaluation state. *)
          let ev' =
            { ev_state = state';
              ev_work = collection'';
              ev_evaluation_steps = step_count;
            }
          in
          (results, ev')
        | Cache_task(key,monadic,continuation) ->
          (* Step monadic value. *)
          let (completed, suspended, blocked, state') =
            _step_m ev.ev_state monadic
          in
          (* Process completed values by executing the continuation and building
             new work from it. *)
          let continued_tasks =
            completed
            |> Enum.map
              (fun value_and_log ->
                 let cache' =
                   Cache_map.add (K(key)) value_and_log search.ss_cache
                 in
                 { ss_task = continuation value_and_log;
                   ss_cache = cache';
                 }
              )
          in
          (* Identify new tasks from suspended and blocked values. *)
          let new_work =
            suspended_and_blocked_states
              (fun m -> Cache_task(key,m,continuation))
              search.ss_cache
              suspended blocked
            |> Enum.map
              (fun task ->
                 { ss_task = task;
                   ss_cache = search.ss_cache;
                 }
              )
            |> Enum.append continued_tasks
            |> Enum.map _search_to_work_item
          in
          (* Build new work collection. *)
          let collection'' =
            new_work
            |> Enum.fold (flip Work_collection.offer) collection'
          in
          (* Build new evaluation state. *)
          let ev' =
            { ev_state = state';
              ev_work = collection'';
              ev_evaluation_steps = step_count;
            }
          in
          (Enum.empty (), ev')
      end
  ;;

  let is_complete (ev : 'out evaluation) : bool =
    Work_collection.is_empty ev.ev_work
  ;;
end;;
