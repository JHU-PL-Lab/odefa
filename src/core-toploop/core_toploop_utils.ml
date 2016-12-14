open Batteries;;

open Core_ast;;
open Ddpa_abstract_ast;;

(** Iterate recursively over all clauses in an expression. *)
let rec iterate_abstract_clauses (Abs_expr(acls)) =
  let top_level = List.enum acls in
  let nested = Enum.delay
      (fun () -> Enum.concat @@
        Enum.map (fun e -> Enum.delay (fun () -> iterate_abstract_clauses e)) @@
        Enum.concat @@ List.enum @@ List.map _abs_exprs_of_clause acls)
  in
  Enum.append top_level nested

and _abs_exprs_of_clause (Abs_clause(_,b)) =
  match b with
  | Abs_conditional_body(_,_,Abs_function_value(_,e1),Abs_function_value(_,e2))
    -> Enum.append (Enum.singleton e1) (Enum.singleton e2)
  | Abs_value_body(v) ->
    _abs_exprs_of_value v
  | Abs_var_body _ | Abs_appl_body _ | Abs_projection_body _ | Abs_deref_body _
  | Abs_update_body _ | Abs_binary_operation_body _
  | Abs_unary_operation_body _ -> Enum.empty ()

and _abs_exprs_of_value v =
  match v with
  | Abs_value_function(Abs_function_value(_,e)) -> Enum.singleton e
  | Abs_value_record _ | Abs_value_ref _ | Abs_value_int | Abs_value_bool _
  | Abs_value_string -> Enum.empty ()
;;

let last_var_of (Expr(cls)) =
  let Clause(x,_) = List.last cls in x
;;
