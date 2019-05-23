type pattern_enum =
  | Pattern_fun
  | Pattern_int
  | Pattern_true
  | Pattern_false
  | Pattern_any
[@@deriving eq, ord]
type binop =
  | Plus
  | Minus
  | Less_than
  | Less_than_or_equal_to
  | Equal_to
  | And
  | Or
  (* | Binary_operator_index
     | Binary_operator_tilde *)
[@@deriving eq, ord]

type symbol = Symbol of Core_ast.Var.t * Core_ast.clause Batteries.Stack.t
type element =
  | Int of int
  | Bool of bool
  | Pattern of pattern_enum

(* the sat result of a formula is a bool, like
   x = 1 (Eq_value)
   x = true
   x = y binop z
   x <= y
   1 <= x Or x <= 1
*)
type formula = 
  | Formula_Not of formula
  (* this may be inaccurate since Z3 needs to know it's int or bool *)
  | Eq_value of symbol * element
  | Eq_symbol of symbol * symbol
  | Eq_binop of symbol * binop * symbol * symbol

type t = formula
type t_phi = formula list

let empty_phi : t_phi = []


let print_phi _ = ()
let string_of_formula _ = ""

let rec string_of_phi phi =
  match phi with
  | [] -> ""
  | head :: tail ->
    (string_of_formula head) ^ " , " ^  (string_of_phi tail)
;;

let eq_value (x: Core_ast.Var.t) (sx : 'a Batteries.Stack.t) (v : Core_ast.value) : t =
  let v' = (match v with 
      | Core_ast.Value_int i -> Int(i)
      | Core_ast.Value_bool b -> Bool(b)
      | _ -> failwith "not support value type in formula"
    ) in
  Eq_value(Symbol(x, Stack.copy sx), v')

let eq_symbol (x: Core_ast.Var.t) (sx : 'a Batteries.Stack.t) (y: Core_ast.Var.t) (sy : 'a Batteries.Stack.t) : t =
  Eq_symbol(Symbol(x, Stack.copy sx), Symbol(y, Stack.copy sy))

let eq_binop (x: Core_ast.Var.t) (sx : 'a Batteries.Stack.t) (op : binop) (y: Core_ast.Var.t) (sy : 'a Batteries.Stack.t) (z: Core_ast.Var.t) (sz : 'a Batteries.Stack.t) : t
  = Eq_binop(
    Symbol(x, Stack.copy sx), op,
    Symbol(y, Stack.copy sy),
    Symbol(z, Stack.copy sz))

(* let string_of_formula *)