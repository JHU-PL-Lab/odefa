(**
   Contains data type definitions for the toy language AST.
*)

open Batteries;;

(** A module for hashtables keyed by UIDs. *)
module Ast_uid_hashtbl = Ast_uid.Ast_uid_hashtbl;;

(** A data type for identifiers in the toy language. *)
type ident = Ident of string;;

module Ident_hash =
struct
  type t = ident
  let equal = (=)
  let hash = Hashtbl.hash
end
;;

module Ident_hashtbl = Hashtbl.Make(Ident_hash);;

module Ident_order =
struct
  type t = ident
  let compare = compare
end
;;

module Ident_set = Set.Make(Ident_order);;

module Ident_map = Map.Make(Ident_order);;

(** A freshening stack of identifiers for variables produced at runtime.  This
    tracks the invocation stack of these variables.  The first element in the
    list is the topmost element in the stack.  If this stack is absent, then
    the variable in question has not been instantiated (and remains within the
    body of a function). *)
type freshening_stack = Freshening_stack of ident list;;

(** Variables in the AST. *)
type var = Var of ident * freshening_stack option;;

module Var_order =
struct
  type t = var
  let compare = compare
end;;

module Var_set = Set.Make(Var_order);;

module Var_map = Map.Make(Var_order);;

module Var_hashtbl = Hashtbl.Make(
  struct
    type t = var
    let equal = (=)
    let hash = Hashtbl.hash
  end
  );;

(** A type to express record values. *)
type record_value = Record_value of var Ident_map.t

(** A type to express function values. *)
and function_value = Function_value of var * expr

(** A type to express reference values. *)
and ref_value = Ref_value of var

(** A type to represent values. *)
and value =
  | Value_record of record_value
  | Value_function of function_value
  | Value_ref of ref_value

(** A type to represent the bodies of clauses. *)
and assignment_clause_body =
  | Value_body of value
  | Var_body of var
  | Appl_body of var * var
  | Conditional_body of var * pattern * function_value * function_value
  | Deref_body of var

(** A type to represent clauses. *)
and clause =
  | Assignment_clause of var * assignment_clause_body
  | Update_clause of var * var

(** A type to represent expressions. *)
and expr = Expr of clause list

(** A type representing conditional patterns. *)
and pattern =
  | Record_pattern of pattern Ident_map.t
;;