(** Defines the monad used by the abstract definitional interpreter. *)

open Batteries;;

open Odefa_ast;;

open Adi_types;;
open Ast;;

module type Sig = sig
  module S : Specification;;
  module T : Adi_structure_types.Sig with module S = S;;
  include Interfaces.Monad
  val with_context : S.C.t -> 'a m -> 'a m
  val allocate : Ident.t -> T.address m
  val read_variable : Ident.t -> T.address m
  val bind_variable : Ident.t -> T.address -> 'a m -> 'a m
  val store_get : T.address -> T.abstract_value m
  val store_set : T.address -> T.abstract_value -> unit m
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
  };;

  type state = {
    st_store : T.Store.t;
  };;

  type 'a m =
    reader -> state -> 'a Enum.t * state
  ;;

  let return (x : 'a) : 'a m =
    fun _read -> fun state -> (Enum.singleton x, state)
  ;;

  let bind (x : 'a m) (f : 'a -> 'b m) : 'b m =
    fun read -> fun state ->
      let (x_value, state') = x read state in
      let y = Enum.map f x_value in
      let ((answers : 'b Enum.t), (state'' : state)) =
        y
        |> Enum.fold
          (fun ((answers : 'b Enum.t), (current_state : state)) (m : 'b m) ->
             let (answer, new_state) = m read current_state in
             (Enum.append answer answers, new_state)
          )
          (Enum.empty (), state')
      in
      (answers, state'')
  ;;

  let with_context (context : S.C.t) (computation : 'a m) : 'a m =
    fun read -> fun state ->
      let read' = { read with read_context = context } in
      computation read' state
  ;;

  let allocate (x : Ident.t) : T.address m =
    fun read -> fun state ->
      let address = T.Address(x, read.read_context) in
      (Enum.singleton address, state)
  ;;

  let read_variable (x : Ident.t) : T.address m =
    fun read -> fun state ->
      let address = Ident_map.find x read.read_environment in
      (Enum.singleton address, state)
  ;;

  let bind_variable (x : Ident.t) (address : T.address) (computation : 'a m)
    : 'a m =
    fun read -> fun state ->
      let env' = Ident_map.add x address read.read_environment in
      let read' = { read with read_environment = env' } in
      computation read' state
  ;;

  let store_get (address : T.address) : T.abstract_value m =
    fun _read -> fun state ->
      (T.Store.find address state.st_store, state)
  ;;

  let store_set (address : T.address) (v : T.abstract_value) : unit m =
    fun _read -> fun state ->
      let store' = T.Store.add address v state.st_store in
      let state' = { st_store = store' } in
      (Enum.singleton (), state')
  ;;

  let run (x : 'a m)
    : 'a Enum.t * (Ident.t -> S.C.t -> T.abstract_value Enum.t) =
    let read =
      { read_context = S.C.empty;
        read_environment = Ident_map.empty;
      }
    in
    let state =
      { st_store = T.Store.empty;
      }
    in
    let (av, state') = x read state in
    let lookup variable_name context =
      let address = T.Address(variable_name, context) in
      T.Store.find address state'.st_store
    in
    (av, lookup)
  ;;
end;;
