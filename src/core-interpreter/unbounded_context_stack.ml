open Batteries;;

open Core_ast;;

type 'a dq = 'a Deque.dq;;

type context_var =
  | Appl_context_var of var option * return_type
  | Cond_context_var of var option * return_type
and return_type = 
  | Return_int of int
  | Return_bool of bool
  | Return_function of function_value * var option * context_var dq
;;

module Unbounded_Stack  =

struct
  (* type t = context_var dq;; *)
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
  let top x =
    match Deque.front x with
    | None -> None
    | Some(c,_) -> Some(c)
  ;;
  let is_empty x =
    match Deque.front x with
    | None -> true
    | Some(_,_) -> false
  ;;
  (* let pp formatter x = *)
    (* pp_concat_sep_delim "" "|?" "|" pp_clause formatter @@ *)
    (* Deque.enum x *)
  (* ;; *)
  (* let show = pp_to_string pp;; *)
end;;
