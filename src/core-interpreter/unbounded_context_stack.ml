open Batteries;;
open Jhupllib;;

(* open Ddpa_abstract_ast;; *)
(* open Ddpa_context_stack;; *)
open Pp_utils;;
open Core_ast;;
open Core_ast_pp;;

type 'a dq = 'a Deque.dq;;

module Unbounded_Stack  =

struct
  type t = clause dq;;
  let equal x y = compare x y == 0;;
  let compare x y = Enum.compare compare (Deque.enum x) (Deque.enum y);;
  let empty = Deque.empty;;
  let push c x =
    Deque.cons c x
  ;;
  let pop x =
    match Deque.front x with
    | None -> Deque.empty
    | Some(_,x') -> x'
  ;;
  let is_top c x =
    match Deque.front x with
    | None -> true
    | Some(c',_) -> c = c'
  ;;
  let get_top x =
    match Deque.front x with
    | None -> None
    | Some(c,_) -> Some(c)
  ;;
  let is_empty x =
    match Deque.front x with
    | None -> true
    | Some(_,_) -> false
  ;;
  let pp formatter x =
    pp_concat_sep_delim "" "|?" "|" pp_clause formatter @@
    Deque.enum x
  ;;
  let show = pp_to_string pp;;
  (* let to_yojson _ = *)
    (* `List [] *)
  (* ;; *)
  (* let name = "context_stack";; *)
end;;
