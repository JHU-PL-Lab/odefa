open Z3.Solver
let context = Z3.mk_context []
let solver = Z3.Solver.mk_solver context None

let xsy = Z3.Symbol.mk_string context "x"
let x = Z3.Boolean.mk_const context xsy

let () = Z3.Solver.add solver [x]

let main () =
  match Z3.Solver.check solver [] with
  | UNSATISFIABLE -> Printf.printf "unsat\n"
  | UNKNOWN -> Printf.printf "unknown"
  | SATISFIABLE ->
    match Z3.Solver.get_model solver with
    | None -> ()
    | Some model ->
      Printf.printf "!!! %s\n"
        (Z3.Model.to_string model)