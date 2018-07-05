(* file that has all the formula related materials *)

open Core_ast;;

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
  | _ -> failwith "TODO"
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
  | _ -> failwith "TODO"
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
