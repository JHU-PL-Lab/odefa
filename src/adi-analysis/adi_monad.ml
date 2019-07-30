(** Defines the monad used by the abstract definitional interpreter. *)

open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Odefa_ast;;

open Abstract_ast;;
open Adi_types;;
open Ast;;
open Logger_utils;;

let lazy_logger = make_lazy_logger "Adi_monad";;

module type Sig = sig
  module S : Specification
  module T : Adi_structure_types.Sig with module S = S;;
  include Interfaces.Monad
  val zero : unit -> 'a m
  val pick : 'a Enum.t -> 'a m
  val sequence : 'a m list -> 'a list m
  val lift : ('a -> 'b) -> 'a m -> 'b m
  val push_context : abstract_clause -> 'a m -> 'a m
  val with_environment : T.environment -> 'a m -> 'a m
  val capture_environment : unit -> T.environment m
  val allocate : Ident.t -> T.address m
  val read_variable : Ident.t -> T.address m
  val bind_variable : Ident.t -> T.address -> 'a m -> 'a m
  val store_get : T.address -> T.abstract_value m
  val store_set : T.address -> T.abstract_value -> unit m
  val lookup : Ident.t -> T.abstract_value m
  val assign : Ident.t -> T.abstract_value -> 'a m -> 'a m
  val cached_at : Ident.t -> T.abstract_value m -> T.abstract_value m
  val run : 'a m -> 'a Enum.t * (Ident.t -> S.C.t -> T.abstract_value Enum.t)
end;;

module Make
    (S : Specification)
    (T : Adi_structure_types.Sig with module S = S)
  : Sig with module S = S and module T = T =
struct
  module S = S;;
  module T = T;;

  type reader = {
    read_context : S.C.t;
    read_environment : T.address Ident_map.t;
  } [@@deriving ord, show];;

  type state = {
    st_store : T.Store.t;
  } [@@deriving ord, show];;

  module Fingerprint = struct
    type t = Ident.t * reader * state [@@deriving ord, show];;
  end;;

  module Cache_set = struct
    module Impl = Set.Make(Fingerprint);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(Fingerprint);;
  end;;
  module Cache_map = struct
    module Impl = Map.Make(Fingerprint)
    include Impl;;
    include Pp_utils.Map_pp(Impl)(Fingerprint);;
  end;;

  type fixpoint_state = {
    fps_visited : Cache_set.t;
    fps_cache : T.Abstract_value_set.t Cache_map.t;
  } [@@deriving show];;
  let _ = pp_fixpoint_state;;

  type 'a m =
    reader -> state -> fixpoint_state -> 'a Enum.t * state * fixpoint_state
  ;;

  let return (x : 'a) : 'a m =
    fun _read state fpstate -> (Enum.singleton x, state, fpstate)
  ;;

  let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    fun read state fpstate ->
      let (x_value, state', fpstate') = x read state fpstate in
      let y = Enum.map f x_value in
      let ((answers : 'b Enum.t),
           (state'' : state),
           (fpstate'' : fixpoint_state)) =
        y
        |> Enum.fold
          (fun ((answers : 'b Enum.t),
                (current_state : state),
                (current_fpstate : fixpoint_state))
            (m : 'b m) ->
            let (answer, new_state, new_fpstate) =
              m read current_state current_fpstate
            in
            (Enum.append answer answers, new_state, new_fpstate)
          )
          (Enum.empty (), state', fpstate')
      in
      (answers, state'', fpstate'')
  ;;

  let zero () = fun _read state fpstate -> (Enum.empty (), state, fpstate);;

  let pick (x : 'a Enum.t) : 'a m =
    fun _read state fpstate -> (x, state, fpstate)
  ;;

  let sequence (xs : 'a m list) : 'a list m =
    let rec loop xs' =
      match xs' with
      | [] -> return []
      | h::t ->
        let%bind h' = h in
        let%bind t' = loop t in
        return @@ h' :: t'
    in
    loop xs
  ;;

  let lift (f : 'a -> 'b) (x : 'a m) : 'b m =
    let%bind x' = x in
    return @@ f x'
  ;;

  let push_context (acl : abstract_clause) (computation : 'a m) : 'a m =
    fun read state fpstate ->
      let context' = S.C.push acl read.read_context in
      let read' = { read with read_context = context' } in
      computation read' state fpstate
  ;;

  let capture_environment () : T.environment m =
    fun read state fpstate -> (Enum.singleton read.read_environment, state, fpstate)
  ;;

  let with_environment (environment : T.environment) (computation : 'a m)
    : 'a m =
    fun read state fpstate ->
      let read' = { read with read_environment = environment } in
      computation read' state fpstate
  ;;

  let allocate (x : Ident.t) : T.address m =
    fun read state fpstate ->
      let address = T.Address(x, read.read_context) in
      (Enum.singleton address, state, fpstate)
  ;;

  let read_variable (x : Ident.t) : T.address m =
    fun read state fpstate ->
      let address = Ident_map.find x read.read_environment in
      (Enum.singleton address, state, fpstate)
  ;;

  let bind_variable (x : Ident.t) (address : T.address) (computation : 'a m)
    : 'a m =
    fun read state fpstate ->
      let env' = Ident_map.add x address read.read_environment in
      let read' = { read with read_environment = env' } in
      computation read' state fpstate
  ;;

  let store_get (address : T.address) : T.abstract_value m =
    fun _read state fpstate ->
      (T.Store.find address state.st_store, state, fpstate)
  ;;

  let store_set (address : T.address) (v : T.abstract_value) : unit m =
    fun _read state fpstate ->
      let store' = T.Store.add address v state.st_store in
      let state' = { st_store = store' } in
      (Enum.singleton (), state', fpstate)
  ;;

  let lookup (x : Ident.t) : T.abstract_value m =
    let%bind address = read_variable x in
    store_get address
  ;;

  let assign (x : Ident.t) (v : T.abstract_value) (computation : 'a m) : 'a m =
    let%bind address = allocate x in
    bind_variable x address @@
    let%bind () = store_set address v in
    computation
  ;;

  let cached_at (x : Ident.t) (computation : T.abstract_value m)
    : T.abstract_value m =
    fun read state fpstate ->
      let fingerprint = (x, read, state) in
      if Cache_set.mem fingerprint fpstate.fps_visited then begin
        let cached_results =
          Cache_map.find_default
            T.Abstract_value_set.empty
            fingerprint
            fpstate.fps_cache
        in
        (T.Abstract_value_set.enum cached_results, state, fpstate)
      end else begin
        let visited' =
          Cache_set.add fingerprint fpstate.fps_visited
        in
        let fpstate' = { fpstate with fps_visited = visited' } in
        let (result, state', fpstate'') = computation read state fpstate' in
        let results = T.Abstract_value_set.of_enum result in
        let cached_results =
          Cache_map.find_default
            T.Abstract_value_set.empty
            fingerprint
            fpstate.fps_cache
        in
        let cache' =
          Cache_map.add fingerprint
            (T.Abstract_value_set.union results cached_results)
            fpstate''.fps_cache
        in
        let fpstate''' = { fpstate'' with fps_cache = cache' } in
        (T.Abstract_value_set.enum results, state', fpstate''')
      end
  ;;

  let run (x : 'a m)
    : 'a Enum.t * (Ident.t -> S.C.t -> T.abstract_value Enum.t) =
    let initial_read =
      { read_context = S.C.empty;
        read_environment = Ident_map.empty;
      }
    in
    let initial_state =
      { st_store = T.Store.empty;
      }
    in
    let rec loop cache =
      lazy_logger `debug (fun () -> "Iterating on analysis");
      let fpstate =
        { fps_visited = Cache_set.empty;
          fps_cache = cache;
        }
      in
      let (av, state', fpstate') = x initial_read initial_state fpstate in
      let cache' = fpstate'.fps_cache in
      if Cache_map.equal T.Abstract_value_set.equal cache cache' then begin
        lazy_logger `debug
          (fun () ->
             Printf.sprintf
               ("Iteration produced no new results.  " ^^
                "Returning with cache\n%s"
               )
               (Cache_map.show T.Abstract_value_set.pp cache')
          );
        (av, state')
      end else begin
        lazy_logger `debug
          (fun () ->
             Printf.sprintf
               ("Iteration produced new results.  Trying to get more...")
          );
        loop cache'
      end
    in
    let (av, state') = loop Cache_map.empty in
    lazy_logger `trace (fun () ->
        T.Store.show state'.st_store
      );
    let lookup variable_name context =
      let address = T.Address(variable_name, context) in
      T.Store.find address state'.st_store
    in
    (av, lookup)
  ;;
end;;
