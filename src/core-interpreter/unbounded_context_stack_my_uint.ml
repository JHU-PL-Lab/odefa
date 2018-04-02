open Batteries;;

open Core_ast;;

type context_var = (var * by_need_type ref) list
and by_need_type =
  | Return_type of return_type
  | Context_var_table of var * context_var
and return_type =
  | Return_int of int
  | Return_uint of int * cont_frame
  | Return_bool of bool
  | Return_function of var * var * value * context_var
and cont_frame =
  | Cont_frame of (var * context_var) list
and continuation_stack = (by_need_type ref option * int * cont_frame) list ;;

module Unbounded_Stack  =

struct
  let empty = [];;

  let push c x = c :: x ;;

  let pop x =
    match x with
    | [] -> []
    | _ :: t -> t
  ;;

  let top x =
    match x with
    | [] -> None
    | h :: _ -> Some(h)
  ;;

  let rec append x1 x2 =
    match x1 with
    | [] -> x2
    | h :: t -> h :: (append t x2)
  ;;
end;;
