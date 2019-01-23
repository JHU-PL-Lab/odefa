(* sat solver. Currently expects formula to be in DNF form *)

open Core_ast;;

let true_formula = Value_formula(Value_bool(true));;
let false_formula = Value_formula(Value_bool(false));;
let formula_to_solve = Binary_formula(false_formula, Binary_operator_bool_or, false_formula);;

let rec separate_clauses (formula:formula) : formula list =
  match formula with
  | Binary_formula(f1, op, f2) ->
    begin
      match op with
      | Binary_operator_plus
      | Binary_operator_int_minus
      | Binary_operator_int_less_than
      | Binary_operator_int_less_than_or_equal_to
      | Binary_operator_equal_to
      | Binary_operator_bool_and
      | Binary_operator_index
      | Binary_operator_tilde ->
        [Binary_formula(f1, op, f2)]
      | Binary_operator_bool_or ->
        separate_clauses f1 @ separate_clauses f2
    end
  | Negated_formula(f) ->
    [Negated_formula(f)]
  | Value_formula(v) ->
    [Value_formula(v)]
  | Var_formula(v) ->
    [Var_formula(v)]
  | Pattern_formula(p) ->
    [Pattern_formula(p)]
;;


let rec solve_clause (clause:formula) : bool =
  match clause with
  (* | Binary_formula(f1, op, f2) -> *)
  | Binary_formula(_,op,_) ->
    begin
      match op with
      | Binary_operator_plus -> true
        (* talk to scott about how to implement this *)
      | Binary_operator_int_minus
      | Binary_operator_int_less_than
      | Binary_operator_int_less_than_or_equal_to
      | Binary_operator_equal_to
      | Binary_operator_bool_and
      | Binary_operator_index
      | Binary_operator_tilde ->
        (* [Binary_formula(f1, op, f2)]; *)
        true
      | Binary_operator_bool_or ->
        (* separate_clauses f1 @ separate_clauses f2; *)
        true
    end
  | Negated_formula(f) ->
    not (solve_clause f)
  | Value_formula(v) ->
    begin
      match v with
      | Value_record(_) ->
        failwith "solve_clause: record encountered in formula"
      | Value_function(_) ->
        failwith "solve_clause: function encountered in formula"
      | Value_ref(_) ->
        failwith "solve_clause: ref encountered in formula"
      | Value_int(_) ->
        true
      | Value_bool(b) ->
        b
    end
  | Var_formula(_) ->
    failwith "TODO"
  | Pattern_formula(_) ->
    failwith "TODO"
;;

let rec solve_helper (clauses:formula list) : bool =
  match clauses with
  | [] ->
    false
  | head :: tail ->
    if solve_clause head then
      true
    else
      solve_helper tail
;;

let solve (formula:formula) : bool =
  let clauses:formula list = separate_clauses formula in
  print_endline ("clauses: " ^ string_of_int (List.length clauses));
  solve_helper clauses
;;

let run () : bool =
  solve formula_to_solve

;;
