(**
   Contains data type definitions for the toy language AST.
*)

open Batteries;;
open Jhupllib;;

(** A data type for identifiers in the toy language. *)
type ident = Ident of string [@@deriving eq, ord, show, to_yojson];;

module Ident =
struct
  type t = ident;;
  let equal = equal_ident;;
  let compare = compare_ident;;
  let pp = pp_ident;;
  let show = show_ident;;
  let to_yojson = ident_to_yojson;;
  let hash = Hashtbl.hash
end;;

module Ident_hashtbl = Hashtbl.Make(Ident);;

module Ident_set = struct
  module S = Set.Make(Ident);;
  include S;;
  include Pp_utils.Set_pp(S)(Ident);;
  include Yojson_utils.Set_to_yojson(S)(Ident);;
end;;

module Ident_map = struct
  module M = Map.Make(Ident);;
  include M;;
  include Pp_utils.Map_pp(M)(Ident);;
  include Yojson_utils.Map_to_yojson(M)(Ident);;
end;;

(** A freshening stack of identifiers for variables produced at runtime.  This
    tracks the invocation stack of these variables.  The first element in the
    list is the topmost element in the stack.  If this stack is absent, then
    the variable in question has not been instantiated (and remains within the
    body of a function). *)
type freshening_stack =
  | Freshening_stack of ident list
[@@deriving eq, ord, to_yojson]
;;

(** Variables in the AST. *)
type var =
  | Var of ident * freshening_stack option
[@@deriving eq, ord, to_yojson]
;;

module Var =
struct
  type t = var;;
  let equal = equal_var;;
  let compare = compare_var;;
  let to_yojson = var_to_yojson;;
  let hash = Hashtbl.hash;;
end;;

module Var_set =
struct
  module S = Set.Make(Var);;
  include S;;
  include Yojson_utils.Set_to_yojson(S)(Var);;
end;;

module Var_map =
struct
  module M = Map.Make(Var);;
  include M;;
  include Yojson_utils.Map_to_yojson(M)(Var);;
end;;

let var_map_union m1 m2 =
  Var_map.fold Var_map.add m2 m1
;;

module Var_hashtbl = Hashtbl.Make(Var);;

type binary_operator =
  | Binary_operator_plus
  | Binary_operator_int_minus
  | Binary_operator_int_less_than
  | Binary_operator_int_less_than_or_equal_to
  | Binary_operator_equal_to
  | Binary_operator_bool_and
  | Binary_operator_bool_or
  | Binary_operator_index
[@@deriving eq, ord]
;;

let show_binary_operator op =
  match op with
  | Binary_operator_plus -> "+"
  | Binary_operator_int_minus -> "-"
  | Binary_operator_int_less_than -> "<"
  | Binary_operator_int_less_than_or_equal_to -> "<="
  | Binary_operator_equal_to -> "=="
  | Binary_operator_bool_and -> "and"
  | Binary_operator_bool_or -> "or"
  | Binary_operator_index -> "@"
;;

let pp_binary_operator formatter op =
  Format.pp_print_string formatter @@ show_binary_operator op
;;

let binary_operator_to_yojson op = `String (show_binary_operator op);;

type unary_operator =
  | Unary_operator_bool_not
  | Unary_operator_bool_coin_flip
[@@deriving eq, ord]
;;

let show_unary_operator op =
  match op with
  | Unary_operator_bool_not ->  "not"
  | Unary_operator_bool_coin_flip ->  "coin_flip"
;;

let pp_unary_operator formatter op =
  Format.pp_print_string formatter @@ show_unary_operator op
;;

let unary_operator_to_yojson op = `String (show_unary_operator op);;

(** A type to express record values. *)
type record_value =
  | Record_value of var Ident_map.t
[@@deriving eq, ord, to_yojson]

(** A type to express function values. *)
and function_value =
  | Function_value of var * expr
[@@deriving eq, ord, to_yojson]

(** A type to express reference values. *)
and ref_value =
  | Ref_value of var
[@@deriving eq, ord, to_yojson]

(** A type to represent values. *)
and value =
  | Value_record of record_value
  | Value_function of function_value
  | Value_ref of ref_value
  | Value_int of int
  | Value_bool of bool
  (* | Value_string of string *)
[@@deriving eq, ord, to_yojson]

(** A type to represent the bodies of clauses. *)
and clause_body =
  | Value_body of value
  | Var_body of var
  | Appl_body of var * var
  | Conditional_body of var * pattern * function_value * function_value
  | Projection_body of var * ident
  | Deref_body of var
  | Update_body of var * var
  | Binary_operation_body of var * binary_operator * var
  | Unary_operation_body of unary_operator * var
  | Input
[@@deriving eq, ord, to_yojson]

(** A type to represent clauses. *)
and clause =
  | Clause of var * clause_body
  (* | Hash of var  *)
[@@deriving eq, ord, to_yojson]

(** A type to represent expressions. *)
and expr = Expr of clause list [@@deriving eq, ord, to_yojson]

(** A type representing conditional patterns. *)
and pattern =
  (* | Record_pattern of pattern Ident_map.t *)
  | Fun_pattern
  (* | Ref_pattern *)
  | Int_pattern
  | Bool_pattern of bool
  | Any_pattern
[@@deriving eq, ord, yojson]
;;

module Value =
struct
  type t = value
  let equal = equal_value;;
  let compare = compare_value;;
  let to_yojson = value_to_yojson;;
end;;

module Pattern =
struct
  type t = pattern
  let equal = equal_pattern;;
  let compare = compare_pattern;;
  let to_yojson = pattern_to_yojson;;
end;;
