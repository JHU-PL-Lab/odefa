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
  val check_formulae : unit -> unit m;;

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

  (* Not currently using state. *)
  type state = {
    st_formulae : Formulae.t;
    st_decisions : decision_map;
    st_steps : int;
  } [@@deriving show];;
  let _ = show_state;;

  (* **** Monad types **** *)

  type 'a m = M of (state -> ('a blockable * state) list)

  (** An unblocked value is either a completed expression (with its resulting
      state) or a suspended function which, when invoked, will step to the next
      result.  The suspended case carries the log of the computation at the time
      it was suspended. *)
  and 'a unblocked =
    | Completed of 'a
    | Suspended of 'a m

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
      blocked_binder : 'a -> 'b m;
      blocked_computation : 'a m;
    }

  (** A blockable value is either a blocked value or an unblocked value. *)
  and 'a blockable =
    | Blocked : ('z, 'a) blocked -> 'a blockable
    | Unblocked : 'a unblocked -> 'a blockable
  ;;

  (* **** Monadic operations **** *)

  let return (type a) (v : a) : a m =
    M(fun state -> ([Unblocked(Completed v), state]))
  ;;

  let zero (type a) () : a m = M(fun _state -> []);;

  (* Recursively binding while keeping the type parameters the same requires
     this little signature trick: a quantified type on the outside and a
     recursive (but monotyped-with-respect-to-type-parameters) declaration on
     the inside. *)
  let bind (type a) (type b) : a m -> (a -> b m) -> b m =
    let rec bind (x : a m) (f : a -> b m) : b m =
      let M(worlds_fn) = x in
      let worlds_fn' (state : state) : (b blockable * state) list =
        let worlds = worlds_fn state in
        worlds
        |> List.enum
        |> Enum.map
          (fun (blockable, state') ->
             match blockable with
             | Unblocked(Completed v) ->
               let M(worlds'_fn) = f v in
               List.enum @@ worlds'_fn state'
             | Unblocked(Suspended m) ->
               Enum.singleton (Unblocked(Suspended (bind m f)), state')
             | Blocked(blocked) ->
               Enum.singleton
                 (Blocked(
                     { blocked_key = blocked.blocked_key;
                       blocked_binder =
                         (* Monadic composition.  In Haskell's "fish" notation:
                            "blocked.blocked_binder >=> f"
                         *)
                         (fun x' -> bind (blocked.blocked_binder x') f);
                       blocked_computation = blocked.blocked_computation;
                     }
                   ), state'
                 )
          )
        |> Enum.concat
        |> List.of_enum
      in
      M(worlds_fn')
    in
    bind
  ;;

  let pick (type a) (items : a Enum.t) : a m =
    M(fun state ->
       (items
        |> Enum.map (fun x -> Unblocked(Completed x), state)
        |> List.of_enum
       )
     )
  ;;

  let pause () : unit m =
    M(fun state ->
       let m =
         M(fun state' ->
            let state'' = { state' with st_steps = state'.st_steps + 1 } in
            [(Unblocked(Completed()), state'')]
          )
       in
       [(Unblocked(Suspended m), state)]
     )
  ;;

  let cache (key : 'a Cache_key.t) (value : 'a m) : 'a m =
    M(fun state ->
       let blocked =
         { blocked_key = key;
           blocked_binder = return;
           blocked_computation = value;
         }
       in
       ([Blocked(blocked), state])
     )
  ;;

  let record_decision
      (s : Relative_stack.t) (x : Ident.t) (c : clause) (x' : Ident.t)
    : unit m =
    M(fun state ->
       match Relative_stack.Map.Exceptionless.find s state.st_decisions with
       | Some(x_,c_,x'_) ->
         if equal_ident x x_ && equal_clause c c_ && equal_ident x' x'_ then
           [(Unblocked(Completed ()), state)]
         else
           []
       | None ->
         let state' =
           { state with
             st_decisions =
               Relative_stack.Map.add s (x,c,x') state.st_decisions
           }
         in
         [(Unblocked(Completed ()), state')]
     )
  ;;

  let record_formula (formula : Formula.t) : unit m =
    M(fun state ->
       try
         let formulae' = Formulae.add formula state.st_formulae in
         [(Unblocked(Completed ()), { state with st_formulae = formulae' })]
       with
       | Formulae.SymbolTypeContradiction _
       | Formulae.SymbolValueContradiction _ ->
         []
     )
  ;;

  let check_formulae () : 'a m =
    M(fun state ->
       if Solver.solvable state.st_formulae then
         [(Unblocked(Completed ()), state)]
       else
         []
     )
  ;;

  (* **** Evaluation module **** *)

  type 'out evaluation_result =
    { er_value : 'out;
      er_formulae : Formulae.t;
      er_evaluation_steps : int;
      er_result_steps : int;
    };;

  module Cache_map = Gmap.Make(Cache_key);;

  type 'out task =
    | Cache_task :
        state * 'a Cache_key.t * 'a m * (state -> 'a -> 'out task) -> 'out task
    | Result_task :
        state * 'out m -> 'out task
  ;;

  type 'out search =
    { ss_cache : Cache_map.t;
      ss_task : 'out task;
    }
  ;;

  type 'out evaluation =
    { ev_work : 'out search Work_collection.t;
      ev_evaluation_steps : int;
    }
  ;;

  type 'a some_blocked = Some_blocked : ('z,'a) blocked -> 'a some_blocked;;

  (* **** Evaluation operations **** *)

  let start (x : 'out m) : 'out evaluation =
    let initial_state =
      { st_formulae = Formulae.empty;
        st_decisions = Relative_stack.Map.empty;
        st_steps = 0;
      }
    in
    let task = Result_task(initial_state, x) in
    let search =
      { ss_cache = Cache_map.empty;
        ss_task = task;
      }
    in
    let work_info =
      { work_item = search;
        work_cache_key = None;
      }
    in
    let collection = Work_collection.offer work_info Work_collection.empty in
    { ev_work = collection;
      ev_evaluation_steps = 0;
    }
  ;;

  let _step_m (type a) (state : state) (x : a m)
    : (a * state) Enum.t *
      (a m * state) Enum.t *
      (a some_blocked * state) Enum.t
    =
    let M(world_fn) = x in
    let worlds = world_fn state in
    let completed, suspended, blocked =
      worlds
      |> List.fold_left
        (fun (completed, suspended, blocked) world ->
           match world with
           | Unblocked(Completed value), state' ->
             ((value, state') :: completed, suspended, blocked)
           | Unblocked(Suspended x'), state' ->
             (completed, (x', state') :: suspended, blocked)
           | Blocked(blocked_item), state' ->
             (completed,
              suspended,
              (Some_blocked blocked_item, state') :: blocked)
        )
        ([], [], [])
    in
    (List.enum completed, List.enum suspended, List.enum blocked)
  ;;

  let suspended_and_blocked_searches
      (type out) (type a)
      (mk_task : a m -> state -> out task)
      (cache : Cache_map.t)
      (suspended : (a m * state) Enum.t)
      (blocked : (a some_blocked * state) Enum.t)
    : out task Enum.t =
    let suspended_states = Enum.map (uncurry mk_task) suspended in
    let blocked_states =
      blocked
      |> Enum.map
        (fun (Some_blocked(blocked), state) ->
           match Cache_map.find blocked.blocked_key cache with
           | None ->
             lazy_logger `trace
               (fun () ->
                  "Cache miss for key " ^ Cache_key.show blocked.blocked_key);
             let continuation (state : state) value : out task =
               let m = blocked.blocked_binder value in
               mk_task m state
             in
             Cache_task(
               state,
               blocked.blocked_key,
               blocked.blocked_computation,
               continuation
             )
           | Some value ->
             lazy_logger `trace
               (fun () ->
                  "Cache hit for key " ^ Cache_key.show blocked.blocked_key);
             mk_task (blocked.blocked_binder value) state
        )
    in
    Enum.append suspended_states blocked_states
  ;;

  let _search_to_work_item search =
    { work_item = search;
      work_cache_key =
        (
          match search.ss_task with
          | Cache_task(_,key,_,_) -> Some(Cache_key.Some_key key)
          | Result_task _ -> None
        );
    }
  ;;

  let _task_state task =
    match task with
    | Cache_task(state,_,_,_) -> state
    | Result_task(state,_) -> state
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
             "Stepping search state for %s with cache size %d and %d formulae"
             (match search.ss_task with
              | Result_task(_,_) -> "result"
              | Cache_task (_, key, _, _) -> Cache_key.show key
             )
             (Cache_map.cardinal search.ss_cache)
             (Formulae.size @@ (_task_state search.ss_task).st_formulae)
        );
      begin
        match search.ss_task with
        | Result_task(state,monadic) ->
          (* Step monadic value. *)
          let (completed, suspended, blocked) = _step_m state monadic in
          (* Process completed values by building evaluation results. *)
          let results : out evaluation_result Enum.t =
            completed
            |> Enum.map
              (fun (value, state) ->
                 { er_value = value;
                   er_formulae = state.st_formulae;
                   er_evaluation_steps = step_count;
                   er_result_steps = state.st_steps + 1;
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
            suspended_and_blocked_searches
              (fun m state -> Result_task(state,m))
              search.ss_cache suspended blocked
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
            { ev_work = collection'';
              ev_evaluation_steps = step_count;
            }
          in
          (results, ev')
        | Cache_task(state,key,monadic,continuation) ->
          (* Step monadic value. *)
          let (completed, suspended, blocked) = _step_m state monadic in
          (* Process completed values by executing the continuation and building
             new work from it. *)
          let continued_tasks =
            completed
            |> Enum.map
              (fun (value, state) ->
                 let cache' =
                   Cache_map.add key value search.ss_cache
                 in
                 { ss_task = continuation state value;
                   ss_cache = cache';
                 }
              )
          in
          (* Identify new tasks from suspended and blocked values. *)
          let new_work =
            suspended_and_blocked_searches
              (fun m state -> Cache_task(state,key,m,continuation))
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
            { ev_work = collection'';
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
