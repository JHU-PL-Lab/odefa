open Batteries;;

open Core_ast;;

module Context_table = Map.Make(Var);;

type context_var = by_need_type ref Context_table.t
and by_need_type =
  | Return_type of return_type
  | Context_var_table of var * context_var
and return_type =
  | Return_int of int
  | Return_uint of int
  | Return_bool of bool
  | Return_function of var * var * value * context_var
;;
