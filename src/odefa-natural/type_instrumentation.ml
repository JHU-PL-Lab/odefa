open Batteries;;

open Odefa_ast;;
open Odefa_symbolic_interpreter;;

open Ast;;

open Interpreter_types;;

open Translator_utils.TranslationMonad;;

let add_type_ab match_clauses op_clause =
  let%bind abort_var = fresh_var "ab" in
  let abort_clause = Clause(abort_var, Abort_body) in
  let%bind () = add_type_abort abort_var match_clauses op_clause in
  return @@ Expr([abort_clause]);
;;

let rec instrument_clauses
    (c_list : clause list)
  : (clause list) m =
  match c_list with
  | clause :: clauses' ->
    begin
      let Clause(v, body) = clause in
      match body with
      | Value_body value ->
        begin
          match value with
          | Value_function f ->
            let Function_value(arg, Expr(body)) = f in
            let%bind new_body = instrument_clauses body in
            let new_fun_val = Function_value(arg, Expr(new_body)) in
            let new_val_body = Value_body(Value_function(new_fun_val)) in
            let new_clause = Clause(v, new_val_body) in
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
      | Abort_body ->
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

            binop = a + b;
            ==>
            m1 = a ~ int;
            m2 = b ~ int;
            m = m1 and m2;
            binop = m ? ( constrain_binop = a + b ) : ( ab = abort );
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
            | Binary_operator_equal_to -> Int_pattern
            | Binary_operator_and
            | Binary_operator_or
            | Binary_operator_xor -> Bool_pattern
          in
          (* Variables *)
          let%bind m1 = fresh_var "m1" in
          let%bind m2 = fresh_var "m2" in
          let%bind m = fresh_var "m" in
          (* Clauses *)
          let m1_clause = Clause(m1, Match_body(v1, pattern)) in
          let m2_clause = Clause(m2, Match_body(v2, pattern)) in
          let and_body = Binary_operation_body(m1, Binary_operator_and, m2) in
          let m_clause = Clause(m, and_body) in
          (* Conditional *)
          let%bind c_binop = fresh_var "constrain_binop" in
          let%bind t_path = return @@ Expr([Clause(c_binop, body)]) in
          let%bind f_path = add_type_ab [m1_clause; m2_clause] clause in
          let cond_clause = Clause(v, Conditional_body(m, t_path, f_path)) in
          let%bind cont = instrument_clauses clauses' in
          return @@ [m1_clause; m2_clause; m_clause; cond_clause] @ cont
        end
      | Projection_body (r, lbl) ->
        begin
          (*
            proj = r.lbl;
            ==>
            m = r ~ {lbl};
            constrain_proj = m ? (proj = r.l) : (ab = abort);
            ==>
            m = r ~ {lbl};
            proj = m ? ( constrain_proj = r.lbl ) : ( ab = abort );
          *)
          (* Pattern match *)
          let%bind m = fresh_var "m" in
          let rec_pat_set = Ident_set.add lbl Ident_set.empty in
          let rec_pat = Rec_pattern rec_pat_set in
          let m_clause = Clause(m, Match_body(r, rec_pat)) in
          (* Conditional *)
          let%bind c_proj = fresh_var "constrain_proj" in
          let%bind t_path = return @@ Expr([Clause(c_proj, body)]) in
          let%bind f_path = add_type_ab [m_clause] clause in
          let cond_clause = Clause(v, Conditional_body(m, t_path, f_path)) in
          let%bind cont = instrument_clauses clauses' in
          return @@ [m_clause; cond_clause] @ cont
        end
      | Appl_body (f, _) ->
        begin
          (*
            appl = f x;
            ==>
            m = f ~ fun;
            constrain_appl = m ? (appl = f x) : (ab = abort);
            ==>
            m = f ~ fun;
            appl = m ? ( constrain_appl = f x ) : ( ab = abort );
          *)
          (* Pattern match *)
          let%bind m = fresh_var "m" in
          let m_clause = Clause(m, Match_body(f, Fun_pattern)) in
          (* Conditional *)
          let%bind c_appl = fresh_var "constrain_appl" in
          let%bind t_path = return @@ Expr([Clause(c_appl, body)]) in
          let%bind f_path = add_type_ab [m_clause] clause in
          let cond_clause = Clause(v, Conditional_body(m, t_path, f_path)) in
          let%bind cont = instrument_clauses clauses' in
          return @@ [m_clause; cond_clause] @ cont
        end
      | Conditional_body (pred, Expr path1, Expr path2) ->
        begin
          (*
            cond = pred ? true_path : false_path;
            ==>
            m = pred ~ bool;
            constrain_cond = m ? (cond = pred ? true_path : false_path)
                               : (ab = abort)
            ==>
            m = pred ~ bool;
            cond = m ? ( constrain_cond = pred ? true_path : false_path )
                     : ( ab = abort );
          *)
          (* Pattern match *)
          let%bind m = fresh_var "m" in
          let m_clause = Clause(m, Match_body(pred, Bool_pattern)) in
          (* Underlying conditional *)
          let%bind path1' = instrument_clauses path1 in
          let%bind path2' = instrument_clauses path2 in
          let body' = Conditional_body(pred, Expr path1', Expr path2') in
          (* Constrain conditional *)
          let%bind c_cond = fresh_var "constrain_cond" in
          let%bind t_path = return @@ Expr([Clause(c_cond, body')]) in
          let%bind f_path = add_type_ab [m_clause] clause in
          let cond_clause = Clause(v, Conditional_body(m, t_path, f_path)) in
          let%bind cont = instrument_clauses clauses' in
          return @@ [m_clause; cond_clause] @ cont
        end
    end
  | [] -> return []
;;

let instrument_odefa (odefa_ast : expr)
  : (expr * abort_info Ident_map.t) =
  let (monad_val : (expr * abort_info Ident_map.t) m) =
    (* Transform odefa program *)
    let Expr(odefa_clist) = odefa_ast in
    let%bind trans_clist = instrument_clauses odefa_clist in
    let%bind odefa_aborts = get_aborts in
    (* Add "~result" to the end of the program *)
    let Clause(last_var, _) = List.last trans_clist in
    let%bind fresh_str = freshness_string in
    let result_var = Ast.Var(Ast.Ident(fresh_str ^ "result"), None) in
    let result_clause = Ast.Clause(result_var, Ast.Var_body(last_var)) in
    return (Expr(trans_clist @ [result_clause]), odefa_aborts)
  in
  let context = Translator_utils.new_translation_context () in
  run context monad_val
;;