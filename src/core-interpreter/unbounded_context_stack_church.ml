open Batteries;;

open Core_ast;;
open Wddpac_graph;;

type context_var = (stack_var * by_need_type ref) list
and stack_var =
  | Stack_var of var
  | Uint_var of uint_clause
and by_need_type =
  | Return_type of return_type
  | Context_var_table of stack_var * context_var
and return_type =
  | Return_int of int
  | Return_uint of int * context_var
  | Return_uint_zero of context_var
  | Return_uint_final_zero
  | Return_uint_add of context_var
  | Return_bool of bool
  | Return_function of var * var * value * context_var
;;

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
end;;
