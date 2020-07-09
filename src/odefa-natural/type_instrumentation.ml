
open Odefa_ast;;
open Ast;;

open On_to_odefa;;
open Translator_utils.TranslationMonad;;

let rec instrument_clauses
    (c_list : Ast.clause list)
  : (Ast.clause list) m =
  match c_list with
  | clause :: clauses' ->
    begin
      let Clause(symb, body) = clause in
      match body with
      | Value_body value ->
        begin
          match value with
          | Value_function f ->
            let Ast.Function_value(arg, Ast.Expr(body)) = f in
            let%bind new_body = instrument_clauses body in
            let new_fun_val = Ast.Function_value(arg, Ast.Expr(new_body)) in
            let new_val_body = Ast.Value_body(Value_function(new_fun_val)) in
            let new_clause = Ast.Clause(symb, new_val_body) in
            let%bind new_clauses' = instrument_clauses clauses' in
            return @@ new_clause :: new_clauses'
          | _ ->
            (* Nothing to constrain *)
            let%bind new_clauses' = instrument_clauses clauses' in
            return @@ clause :: new_clauses'
        end
      | Var_body _
      | Input_body
      | Match_body _
      | Abort_body _ ->
        (* Nothing to constrain *)
        begin
          let%bind new_clauses' = instrument_clauses clauses' in
          return @@ clause :: new_clauses'
        end
      | Binary_operation_body (v1, binop, v2) ->
        begin
          (*
            binop = a + b;
            ==>
            m1 = a ~ int;
            m2 = b ~ int;
            m = m1 and m2;
            constrain_binop = m ? (binop = a + b) : (ab = abort);
          *)
          let pattern =
            match binop with
            | Binary_operator_plus
            | Binary_operator_minus
            | Binary_operator_times
            | Binary_operator_divide
            | Binary_operator_modulus
            | Binary_operator_less_than
            | Binary_operator_less_than_or_equal_to
            (* TODO: Make "==" binop work with both ints and bools *)
            | Binary_operator_equal_to -> Ast.Int_pattern
            | Binary_operator_and
            | Binary_operator_or
            | Binary_operator_xor -> Ast.Bool_pattern 
          in
          (* Variables *)
          let%bind m1 = fresh_var "m1" in
          let%bind m2 = fresh_var "m2" in
          let%bind m = fresh_var "m" in
          let%bind v = fresh_var "constrain_binop" in
          (* Clauses *)
          let m1_clause = Ast.Clause(m1, Match_body(v1, pattern)) in
          let m2_clause = Ast.Clause(m2, Match_body(v2, pattern)) in
          (* Let-bind operator to stay under 80 lines *)
          let and_op = Ast.Binary_operator_and in
          let binop_body = Ast.Binary_operation_body(m1, and_op, m2) in
          let m_clause = Ast.Clause(m, binop_body) in
          let%bind new_clauses' = instrument_clauses clauses' in
          let%bind t_path = return @@ Ast.Expr(clause :: new_clauses') in
          let%bind f_path = get_abort_expr_2 clause [m1_clause; m2_clause] in
          let val_clause
            = Ast.Clause(v, Conditional_body(m, t_path, f_path))
          in
          return @@ [m1_clause; m2_clause; m_clause; val_clause]
        end
      | Projection_body (r, lbl) ->
        begin
          (*
            proj = r.l;
            ==>
            m = r ~ {l};
            constrain_proj = m ? (proj = r.l) : (ab = abort);
          *)
          let%bind m = fresh_var "m" in
          let%bind v = fresh_var "constrain_proj" in
          let rec_pat_set =
            Ast.Ident_set.add lbl Ast.Ident_set.empty
          in
          let rec_pat = Ast.Rec_pattern rec_pat_set in
          let m_clause = Ast.Clause(m, Match_body(r, rec_pat)) in
          let%bind new_clauses' = instrument_clauses clauses' in
          let%bind t_path = return @@ Ast.Expr(clause :: new_clauses') in
          let%bind f_path = get_abort_expr_2 clause [m_clause] in
          let val_clause
            = Ast.Clause(v, Conditional_body(m, t_path, f_path))
          in
          return @@ [m_clause; val_clause]
        end
      | Appl_body (f, _) ->
        begin
          (*
            appl = f x;
            ==>
            m = f ~ fun;
            constrain_appl = m ? (appl = f x) : (ab = abort);
          *)
          let%bind m = fresh_var "m" in
          let%bind v = fresh_var "constrain_appl" in
          let m_clause = Ast.Clause(m, Match_body(f, Fun_pattern)) in
          let%bind new_clauses' = instrument_clauses clauses' in
          let%bind t_path = return @@ Ast.Expr(clause :: new_clauses') in
          let%bind f_path = get_abort_expr_2 clause [m_clause] in
          let val_clause
            = Ast.Clause(v, Conditional_body(m, t_path, f_path))
          in
          return @@ [m_clause; val_clause]
        end
      | Conditional_body (pred, Ast.Expr path1, Ast.Expr path2) ->
        begin
          (*
            cond = pred ? true_path : false_path;
            ==>
            m = pred ~ bool;
            constrain_cond = m ? (cond = pred ? true_path : false_path)
                               : (ab = abort)
          *)
          let%bind m = fresh_var "m" in
          let%bind v = fresh_var "constrain_cond" in
          let m_clause = Ast.Clause(m, Match_body(pred, Bool_pattern)) in
          let%bind new_clauses' = instrument_clauses clauses' in
          let%bind new_path1 = instrument_clauses path1 in
          let%bind new_path2 = instrument_clauses path2 in
          let new_cond_body =
            Ast.Conditional_body(pred, Expr new_path1, Expr new_path2)
          in
          let new_clause = Ast.Clause(symb, new_cond_body) in
          let%bind t_path = return @@ Ast.Expr(new_clause :: new_clauses') in
          let%bind f_path = get_abort_expr_2 clause [m_clause] in
          let val_clause
            = Ast.Clause(v, Conditional_body(m, t_path, f_path))
          in
          return @@ [m_clause; val_clause]
        end
    end
  | [] -> return []
;;

let instrument_odefa (odefa_ast : Ast.expr)
  : (Odefa_ast.Ast.expr * On_to_odefa_types.odefa_natodefa_info) =
  let (monad_val : (Ast.expr * On_to_odefa_types.odefa_natodefa_info) m) =
    let Ast.Expr(odefa_clist) = odefa_ast in
    let%bind trans_clist = condition_clauses odefa_clist in
    let%bind odefa_info = get_odefa_natodefa_info in
    return (Ast.Expr(trans_clist), odefa_info)
  in
  let context = Translator_utils.new_translation_context () in
  run context monad_val
;;