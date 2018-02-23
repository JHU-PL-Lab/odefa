open Batteries;;

open Core_ast;;

type context_var = (var * by_need_type ref) list
and by_need_type =
  | Return_type of return_type
  | Context_var_table of var * context_var
and return_type =
  | Return_int of int
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
