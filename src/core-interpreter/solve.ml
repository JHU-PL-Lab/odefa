(* open Z3
   open Z3.Boolean
   open Z3.Arithmetic
   open Batteries
   open Core_interpreter_utils
   open Core_ast
*)

(* let symbol_of_formula (f : Formula_type.t) : string option =
   match f with
   | Var_formula(Var(Ident(s), _), context_stack) -> (
      let name = s ^ "AAA" ^ (string_of_context_stack context_stack) ^ "AAA" in
      Some(name)
    )
   | _ -> None

   let int_of_formula (f : Formula_type.t) : int option =
   match f with
   | Value_formula(Value_int(i)) -> Some(i)
   | _ -> None

   let expression_of_formula (ctx : context) (f : Formula_type.t) : Expr.expr = 
   match f with
   | Binary_formula(f1, _, f2) -> (
      match (symbol_of_formula f1, int_of_formula f2) with
      | Some s, Some i -> (
          let e = mk_eq ctx (Integer.mk_const_s ctx s) (Integer.mk_numeral_i ctx i)
          in e
        )
      | _ -> failwith "not implemented"
    )
   | _ -> failwith "not implemented" *)

let check (phi : Formula_type.t_phi)  =
  phi
(*   let context = Z3.mk_context [] in
     let solver = Solver.mk_solver context None in
     ()
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
     ) *)

