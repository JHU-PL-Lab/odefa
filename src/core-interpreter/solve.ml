open Z3
open Z3.Boolean
open Z3.Arithmetic
(* open Z3.Arithmetic.Real *)
open Core_ast
open Core_interpreter_utils


(* 
Here is the lists of added formula:

line 218, Value Discovery
Binary_formula(Var_formula(x, (Stack.copy context_stack)), Binary_operator_equal_to, Value_formula(v))]
x = 1

line 229, Alias
Binary_formula(Var_formula(cur_var, (Stack.copy context_stack)),Binary_operator_equal_to, Var_formula(var, (Stack.copy context_for_var)))
x = y

line 235, Input
let negCover = Binary_formula(Var_formula(x, (Stack.copy context_stack)),Binary_operator_int_less_than_or_equal_to, Value_formula(Value_int(1))) in
let posCover = Binary_formula(Value_formula(Value_int(1)), Binary_operator_int_less_than_or_equal_to, Var_formula(x, (Stack.copy context_stack))) in
let existence_formula = Binary_formula(negCover, Binary_operator_bool_or, posCover)
(x <= 1)  Or (1 <= x)

line 250, Unary
Binary_formula(Var_formula(cur_var, (Stack.copy context_stack)),Binary_operator_equal_to, Negated_formula(Var_formula(v1, (Stack.copy context_stack))))
x = not y

line 300, Binary
not generated here

line 307, Binary
Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)), Binary_operator_equal_to,
                                                               Binary_formula(Var_formula(v1, (Stack.copy context_for_left)), Binary_operator_plus, Var_formula(v2, (Stack.copy context_for_right))))
...
x = y + z
x = y - z
x = y < z
x = y <= z
x = y = z
x = y && z
x = y || z

line 416, Enter
Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),Binary_operator_equal_to, Var_formula(xn, (Stack.copy context_stack)))
x = y

line 441/502, Conditional body
let pattern_formula = Binary_formula(Var_formula(a, (Stack.copy context_stack)), Binary_operator_equal_to, Pattern_formula(p)) in
  if xf1 = cur_node_var then
    Binary_formula(pattern_formula, Binary_operator_equal_to, Value_formula(Value_bool(true)))
  else
    Binary_formula(pattern_formula, Binary_operator_equal_to, Value_formula(Value_bool(false)))

fun/true/false = 1

line 517, Conditional body
Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),
  Binary_operator_equal_to, Var_formula(arg, (Stack.copy context_stack)))
x = y

line 541, Exit
Binary_formula(Var_formula(cur_var, (Stack.copy original_context_stack)),
  Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy context_stack)))
x = y

line 631, Exit
let new_phi_1 = new_phi@[Binary_formula(Var_formula(x, (Stack.copy context_stack)), Binary_operator_equal_to, Pattern_formula(p))]
  @[Binary_formula(Var_formula(original_program_point, (Stack.copy original_context_stack)),
    Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy original_context_stack_2)))]
in
let new_phi_2 = new_phi@[Negated_formula(Binary_formula(Var_formula(x, (Stack.copy context_stack)), Binary_operator_equal_to, Pattern_formula(p)))]
  @[Binary_formula(Var_formula(false_branch_original_program_point, (Stack.copy original_context_stack)),
    Binary_operator_equal_to, Var_formula(false_branch_new_program_point, (Stack.copy original_context_stack_3)))]

x = fun
x = y

Binary_formula(Var_formula(original_program_point, (Stack.copy context_stack)),
  Binary_operator_equal_to, Var_formula(new_program_point, (Stack.copy context_stack)))

x = y
 *)

let symbol_of_formula (f : formula) : string option =
  match f with
  | Var_formula(Var(Ident(s), _), context_stack) -> (
      let name = s ^ "AAA" ^ (string_of_context_stack context_stack) ^ "AAA" in
      Some(name)
    )
  | _ -> None

let int_of_formula (f : formula) : int option =
  match f with
  | Value_formula(Value_int(i)) -> Some(i)
  | _ -> None

let expression_of_formula (ctx : context) (f : formula) : Expr.expr = 
  match f with
  | Binary_formula(f1, _, f2) -> (
      match (symbol_of_formula f1, int_of_formula f2) with
      | Some s, Some i -> (
          let e = mk_eq ctx (Integer.mk_const_s ctx s) (Integer.mk_numeral_i ctx i)
          in e
        )
      | _ -> failwith "not implemented"
    )
  | _ -> failwith "not implemented"

let check (phi : formula list)  =
  let context = Z3.mk_context [] in
  let solver = Solver.mk_solver context None in
  let exps = List.map (expression_of_formula context) phi in 
  Solver.add solver exps;
  match Solver.check solver [] with
  | Solver.SATISFIABLE -> (
      match Solver.get_model solver with
      | None -> failwith "failure: impossible none model"
      | Some model ->
        Printf.printf "!!! %s\n"
          (Model.to_string model))
  | Solver.UNSATISFIABLE -> (
      failwith "failure: unsat in solve.check"
    )
  | Solver.UNKNOWN -> (
      print_endline (Solver.get_reason_unknown solver);
      failwith "failure: unknown in solve.check"
    )

