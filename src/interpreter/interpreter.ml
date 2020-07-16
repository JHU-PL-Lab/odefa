open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Pp_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

type evaluation_environment = (value option) Environment.t;;

let pp_value_option formatter val_opt =
  match val_opt with
  | Some v -> Format.fprintf formatter "%a" pp_value v
  | None -> Format.pp_print_string formatter "undefined"
;;

let pp_evaluation_environment = pp_map pp_var pp_value_option Environment.enum;;
let show_evaluation_environment = pp_to_string pp_evaluation_environment;;

exception Evaluation_failure of string;;

let lookup env x =
  if Environment.mem env x then
    Environment.find env x
  else
    raise @@ Evaluation_failure (
        Printf.sprintf "cannot find variable %s in environment %s."
          (show_var x) (show_evaluation_environment env)
      )
;;

(* FIXME: this functionality is duplicated in ast_wellformedness.
   (Needs fixed upstream.) *)
let rec bound_vars_of_expr (Expr(cls)) =
  cls
  |> List.map
    (fun (Clause(x, b)) ->
       Var_set.add x @@
       match b with
       | Conditional_body(_,e1,e2) ->
         Var_set.union (bound_vars_of_expr e1) (bound_vars_of_expr e2)
       | _ -> Var_set.empty
    )
  |> List.fold_left Var_set.union Var_set.empty
;;

let rec var_replace_expr fn (Expr(cls)) =
  Expr(List.map (var_replace_clause fn) cls)

and var_replace_clause fn (Clause(x, b)) =
  Clause(fn x, var_replace_clause_body fn b)

and var_replace_clause_body fn r =
  match r with
  | Value_body(v) -> Value_body(var_replace_value fn v)
  | Var_body(x) -> Var_body(fn x)
  | Input_body -> Input_body
  | Appl_body(x1, x2) -> Appl_body(fn x1, fn x2)
  | Conditional_body(x,e1,e2) ->
    Conditional_body(fn x, var_replace_expr fn e1, var_replace_expr fn e2)
  | Match_body(x,p) ->
    Match_body(fn x,p)
  | Projection_body(x,l) ->
    Projection_body(fn x,l)
  | Binary_operation_body(x1,op,x2) -> Binary_operation_body(fn x1, op, fn x2)
  | Abort_body -> Abort_body

and var_replace_value fn v =
  match v with
  | Value_record(Record_value m) ->
    Value_record(Record_value(Ident_map.map fn m))
  | Value_function(f) -> Value_function(var_replace_function_value fn f)
  | Value_int n -> Value_int n
  | Value_bool b -> Value_bool b

and var_replace_function_value fn (Function_value(x, e)) =
  Function_value(fn x, var_replace_expr fn e)

let freshening_stack_from_var x =
  let Var(appl_i, appl_fso) = x in
  (* The freshening stack of a call site at top level is always
     present. *)
  let Freshening_stack idents = Option.get appl_fso in
  Freshening_stack (appl_i :: idents)
;;

let repl_fn_for clauses freshening_stack extra_bound =
  let bound_variables =
    bound_vars_of_expr clauses
    |> Var_set.union extra_bound
  in
  let repl_fn (Var(i, _) as x) =
    if Var_set.mem x bound_variables
    then Var(i, Some freshening_stack)
    else x
  in
  repl_fn
;;

let fun_wire (Function_value(param_x, body_expr)) arg_x call_site_x =
  (* Build the variable freshening function. *)
  let freshening_stack = freshening_stack_from_var call_site_x in
  let repl_fn =
    repl_fn_for body_expr freshening_stack @@ Var_set.singleton param_x in
  (* Create the freshened, wired body. *)
  let Expr(freshened_body) = var_replace_expr repl_fn body_expr in
  let head_clause = Clause(repl_fn param_x, Var_body(arg_x)) in
  let Clause(last_var,_) = List.last freshened_body in
  let tail_clause = Clause(call_site_x, Var_body(last_var)) in
  [head_clause] @ freshened_body @ [tail_clause]
;;

let cond_wire (conditional_site_x : var) (Expr(body)) =
  let Clause(last_var, _) = List.last body in
  let tail_clause = Clause(conditional_site_x, Var_body(last_var)) in
  body @ [tail_clause]
;;

let stdin_input_source (_:var) = Value_int(read_int());;

let matches env x p : bool =
  let val_opt = lookup env x in
  match val_opt with
  | Some v ->
    begin
      match v, p with
      | (_, Any_pattern)
      | (Value_function(Function_value(_)), Fun_pattern)
      | (Value_int _, Int_pattern)
      | (Value_bool _, Bool_pattern) -> true
      | (Value_record(Record_value(record)), Rec_pattern p_record) ->
        begin
          let p_enum = Ident_set.enum p_record in
          let record_keys = Ident_set.of_enum @@ Ident_map.keys record in
          Enum.for_all (fun ident -> Ident_set.mem ident record_keys) p_enum
        end
      | _ -> false
    end
  (* Since None has "type" of bottom, it cannot match any pattern *)
  | None -> false
;;

let fail_on_abort (Clause(ab_var, _)) : unit =
  lazy_logger `trace (fun () -> Printf.sprintf "Aborting %s" (Ast_pp.show_var ab_var));
  raise @@ Evaluation_failure
    (Printf.sprintf "Evaluation fails on abort clause at %s" (show_var ab_var))
;;

let rec evaluate
    ?input_source:(input_source=stdin_input_source)
    ?clause_callback:(clause_callback=fun (_:clause) -> ())
    ?abort_policy:(abort_policy=fail_on_abort)
    env
    lastvar
    cls =
  lazy_logger `debug (fun () ->
      Format.asprintf
        "\nEnvironment: @[%a@]\nLast var:    @[%a@]\nClauses:     @[%a@]\n"
        pp_evaluation_environment env
        (Pp_utils.pp_option pp_var) lastvar
        pp_expr (Expr(cls)));
  flush stdout;
  match cls with
  | [] ->
    begin
      match lastvar with
      | Some(x) -> (x, env)
      | None ->
        (* TODO: different exception? *)
        raise (Failure "evaluation of empty expression!")
    end
  | (Clause(x, b) as c):: t ->
    begin
      let recurse =
        evaluate
          ~input_source:input_source
          ~clause_callback:clause_callback
          ~abort_policy:abort_policy
          env
          (Some x)
      in
      clause_callback c;
      match b with
      | Value_body v ->
        Environment.add env x (Some v);
        recurse t
      | Var_body x' ->
        let v_opt = lookup env x' in
        Environment.add env x v_opt;
        recurse t
      | Input_body ->
        let v = input_source x in
        Environment.add env x (Some v);
        recurse t
      | Appl_body (x', x'') ->
        begin
          match lookup env x' with
          | Some (Value_function f) ->
            recurse @@ fun_wire f x'' x @ t
          | Some r ->
            raise @@ Evaluation_failure
              (Printf.sprintf
                "cannot apply %s as it contains non-function %s"
                (show_var x') (show_value r))
          | None ->
            raise @@ Evaluation_failure
              (Printf.sprintf "cannot apply %s to undefined value"
                (show_var x))
        end
      | Conditional_body (x', e1, e2) ->
        let v_opt = lookup env x' in
        let e_target =
          match v_opt with
          | Some (Value_bool b) -> if b then e1 else e2
          | Some v ->
            raise @@ Evaluation_failure
              (Printf.sprintf
                "cannot condition on non-boolean value %s" (show_value v))
          | None ->
            raise @@ Evaluation_failure
              (Printf.sprintf "cannot condition on undefined value")

        in
        recurse @@ cond_wire x e_target @ t
      | Match_body (x', p) ->
        let result = Value_bool (matches env x' p) in
        Environment.add env x (Some result);
        recurse t
      | Projection_body (x', l) ->
        begin
          match lookup env x' with
          | Some (Value_record (Record_value els)) ->
            begin
              try
                let x'' = Ident_map.find l els in
                let v_opt = lookup env x'' in
                Environment.add env x v_opt;
                recurse t
              with
              | Not_found ->
                raise @@ Evaluation_failure(
                  Printf.sprintf "cannot project %s from %s: not present"
                    (show_ident l)
                    (show_value (Value_record (Record_value els))))
            end
          | Some v ->
            raise @@ Evaluation_failure(
              Printf.sprintf "cannot project %s from non-record value %s"
                (show_ident l) (show_value v))
          | None ->
            raise @@ Evaluation_failure
              (Printf.sprintf "cannot project %s from undefined value"
                (show_ident l))
        end
      | Binary_operation_body (x1, op, x2) ->
        let v1_opt = lookup env x1 in
        let v2_opt = lookup env x2 in
        let result =
          begin
            match v1_opt, v2_opt with
            | Some v1, Some v2 ->
              begin
                match v1, op, v2 with
                | (Value_int n1, Binary_operator_plus, Value_int n2) ->
                  Value_int (n1 + n2)
                | (Value_int n1, Binary_operator_minus, Value_int n2) ->
                  Value_int (n1 - n2)
                | (Value_int n1, Binary_operator_times, Value_int n2) ->
                  Value_int (n1 * n2)
                | (Value_int n1, Binary_operator_divide, Value_int n2) ->
                  if n2 <> 0 then Value_int (n1 / n2) else
                    raise @@ Evaluation_failure
                      ("Divide by zero at " ^ show_var x)
                | (Value_int n1, Binary_operator_modulus, Value_int n2) ->
                  if n2 <> 0 then Value_int(n1 mod n2) else
                    raise @@ Evaluation_failure
                      ("Modulus by zero at " ^ show_var x)
                | (Value_int n1, Binary_operator_less_than, Value_int n2) ->
                  Value_bool (n1 < n2)
                | (Value_int n1,
                  Binary_operator_less_than_or_equal_to,
                  Value_int n2) ->
                  Value_bool (n1 <= n2)
                | (Value_int n1, Binary_operator_equal_to, Value_int n2) ->
                  Value_bool (n1 = n2)
                | (Value_bool b1, Binary_operator_equal_to, Value_bool b2) ->
                  Value_bool (b1 = b2)
                | (Value_bool b1, Binary_operator_and, Value_bool b2) ->
                  Value_bool (b1 && b2)
                | (Value_bool b1, Binary_operator_or, Value_bool b2) ->
                  Value_bool (b1 || b2)
                | (Value_bool b1, Binary_operator_xor, Value_bool b2) ->
                  Value_bool (b1 <> b2)
                | _, _, _ ->
                  raise @@ Evaluation_failure
                    (Printf.sprintf
                      "Cannot complete binary operation: (%s) %s (%s)"
                      (show_value v1) (show_binary_operator op) (show_value v2))
              end
            | None, Some _ | Some _, None | None, None ->
              raise @@ Evaluation_failure
                (Printf.sprintf
                  "cannot complete binary operation due to undefined values")
          end
        in
        Environment.add env x (Some result);
        recurse t
      | Abort_body ->
        abort_policy c;
        (* Unreachable code with default abort policy *)
        Environment.add env x None;
        recurse t
    end
;;

let eval
    ?input_source:(input_source=stdin_input_source)
    ?clause_callback:(clause_callback=fun (_:clause) -> ())
    ?abort_policy:(abort_policy=fail_on_abort)
    e
  =
  let env = Environment.create(20) in
  let repl_fn = repl_fn_for e (Freshening_stack []) Var_set.empty in
  let Expr(cls) = var_replace_expr repl_fn e in
  evaluate
    ~input_source:input_source
    ~clause_callback:clause_callback
    ~abort_policy:abort_policy
    env
    None
    cls
;;
