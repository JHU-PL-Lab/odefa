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

(** A type to express empty value sets. *)
(* and empty_value =
   | Empty_value
   [@@deriving eq, ord, to_yojson] *)

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



(* formula type that will be passed along as a parameter in lookup
   couple things I'm thinking of:
   what values to I need?
   method to check if its valid
   pretty sure don't need a map to associate x with formula
   substitute vars in formula method
*)
type formula =
  | Binary_formula of formula * binary_operator * formula
  | Negated_formula of formula
  | Value_formula of value
  | Var_formula of var
  | Pattern_formula of pattern
;;

let true_formula = Value_formula(Value_bool(true));;

(* substitutes instances of x with x' *)
let rec substitute_var formula x (x':var) : formula =
  match formula with
  | Binary_formula(f1, op, f2) -> Binary_formula(substitute_var f1 x x', op, substitute_var f2 x x')
  | Negated_formula(f1) -> Negated_formula(substitute_var f1 x x')
  | Value_formula(v) -> Value_formula(v)
  | Var_formula(v) ->
    if v = x then
      Var_formula(x')
    else
      Var_formula(v)
  | Pattern_formula(p) -> Pattern_formula(p)
;;

(* substitutes instances of x with v1 *)
let rec substitute_value formula x (v1:value) : formula =
  match formula with
  | Binary_formula(f1, op, f2) -> Binary_formula(substitute_value f1 x v1, op, substitute_value f2 x v1)
  | Negated_formula(f1) -> Negated_formula(substitute_value f1 x v1)
  | Value_formula(v) -> Value_formula(v)
  | Var_formula(v) ->
    if v = x then
      Value_formula(v1)
    else
      Var_formula(v)
  | Pattern_formula(p) -> Pattern_formula(p)
;;

(* substitutes instances of var_formula x with f *)
let rec substitute_formula formula x (f:formula) : formula =
  match formula with
  | Binary_formula(f1, op, f2) -> Binary_formula(substitute_formula f1 x f, op, substitute_formula f2 x f)
  | Negated_formula(f1) -> Negated_formula(substitute_formula f1 x f)
  | Value_formula(v) -> Value_formula(v)
  | Var_formula(v) ->
    if v = x then
      f
    else
      Var_formula(v)
  | Pattern_formula(p) -> Pattern_formula(p)
;;

(* formula to string converter *)
let rec string_of_formula formula : string =
  match formula with
  | Binary_formula(f1, op, f2) ->
    begin
      match op with
      | Binary_operator_plus ->
        string_of_formula f1 ^ " + " ^ string_of_formula f2
      | Binary_operator_int_minus ->
        string_of_formula f1 ^ " - " ^ string_of_formula f2
      | Binary_operator_int_less_than ->
        string_of_formula f1 ^ " < " ^ string_of_formula f2
      | Binary_operator_int_less_than_or_equal_to ->
        string_of_formula f1 ^ " <= " ^ string_of_formula f2
      | Binary_operator_equal_to ->
        string_of_formula f1 ^ " == " ^ string_of_formula f2
      | Binary_operator_bool_and ->
        string_of_formula f1 ^ " && " ^ string_of_formula f2
      | Binary_operator_bool_or ->
        string_of_formula f1 ^ " || " ^ string_of_formula f2
      | Binary_operator_index ->
        string_of_formula f1 ^ " . " ^ string_of_formula f2
    end
  | Negated_formula(f1) -> "not " ^ string_of_formula f1
  | Value_formula(v) -> "value " ^
                        (match v with
                         | Value_record(_) -> "record"
                         | Value_function(_) -> "function"
                         | Value_ref(_) -> "ref"
                         | Value_int(i) -> string_of_int i
                         | Value_bool(b) -> string_of_bool b
                        )
  | Var_formula(var) ->
    begin
      match var with
      | Var(i, _) ->
        begin
          match i with
          | Ident(s) -> "variable " ^ s
        end
    end
  | _ -> failwith "TODO"
;;

let pp_formula _ formula = string_of_formula formula;;

(* let pp_hashtbl _ iota =
  let temp = Hashtbl.length iota in
  string_of_int temp
;; *)

type int_or_bool = Int of int | Bool of bool;;

(* need method that checks if the formula is satisfiable (not technical, but in this context)
   kind of hard because I really want to evaluate the formula but there are both
   ints and bools in the intermediate steps of the evaluation. need to bundle ints
   and bools in one type so the return type is okay. check outside this method for whether or not
   the formula is ok
*)
let rec check_formula_helper formula : int_or_bool =
  match formula with
  | Binary_formula(f1,op,f2) ->
    let r1 = check_formula_helper f1 in
    let r2 = check_formula_helper f2 in
    begin
      match op with
      | Binary_operator_plus ->
        begin
          match r1 with
          | Int(i1) ->
            begin
              match r2 with
              | Int(i2) -> Int(i1+i2)
              | Bool(_) -> Bool(false)
            end
          | Bool(_) ->
            Bool(false)
        end
      | Binary_operator_int_minus ->
        begin
          match r1 with
          | Int(i1) ->
            begin
              match r2 with
              | Int(i2) -> Int(i1-i2)
              | Bool(_) -> Bool(false)
            end
          | Bool(_) ->
            Bool(false)
        end
      | Binary_operator_int_less_than ->
        begin
          match r1 with
          | Int(i1) ->
            begin
              match r2 with
              | Int(i2) -> Bool(i1 < i2)
              | Bool(_) -> Bool(false)
            end
          | Bool(_) ->
            Bool(false)
        end
      | Binary_operator_int_less_than_or_equal_to ->
        begin
          match r1 with
          | Int(i1) ->
            begin
              match r2 with
              | Int(i2) -> Bool(i1 <= i2)
              | Bool(_) -> Bool(false)
            end
          | Bool(_) ->
            Bool(false)
        end
      | Binary_operator_equal_to ->
        begin
          match r1 with
          | Int(i1) ->
            begin
              match r2 with
              | Int(i2) -> Bool(i1 = i2)
              | Bool(_) -> Bool(false)
            end
          | Bool(b1) ->
            begin
              match r2 with
              | Int(_) -> Bool(false)
              | Bool(b2) -> Bool(b1 = b2)
            end
        end
      | Binary_operator_bool_and ->
        begin
          match r1 with
          | Int(_) ->
            Bool(false)
          | Bool(b1) ->
            begin
              match r2 with
              | Int(_) -> Bool(false)
              | Bool(b2) -> Bool(b1 && b2)
            end
        end
      | Binary_operator_bool_or ->
        begin
          match r1 with
          | Int(_) ->
            Bool(false)
          | Bool(b1) ->
            begin
              match r2 with
              | Int(_) -> Bool(false)
              | Bool(b2) -> Bool(b1 || b2)
            end
        end
      | Binary_operator_index ->
        Bool(false)
    end
  | Negated_formula(f1) ->
    let r1 = check_formula_helper f1 in
    begin
      match r1 with
      | Int(_) -> Bool(false)
      | Bool(b) -> Bool(not b)
    end
  | Value_formula(v) ->
    begin
      match v with
      | Value_int(i) -> Int(i)
      | Value_bool(b) -> Bool(b)
      | _ -> failwith "not supported"
    end
  | Var_formula(_) -> failwith "never should have happened"
  | _ -> failwith "TODO"
;;

let check_formula formula : bool =
  match check_formula_helper formula with
  | Int(_) -> true
  | Bool(b) -> b
;;
