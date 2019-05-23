open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Pp_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

type evaluation_environment = value Environment.t;;

let pp_evaluation_environment = pp_map pp_var pp_value Environment.enum;;
let show_evaluation_environment = pp_to_string pp_evaluation_environment;;

exception Evaluation_failure of string;;

let lookup env x =
  if Environment.mem env x then
    Environment.find env x
  else
    raise (
      Evaluation_failure (
        Printf.sprintf "cannot find variable %s in environment %s."
          (show_var x) (show_evaluation_environment env)
      )
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
  | Binary_operation_body(x1,op,x2) -> Binary_operation_body(fn x1, op, fn x2)

and var_replace_value fn v =
  match v with
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

let rec evaluate env lastvar cls =
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
  | (Clause(x, b)):: t ->
    begin
      match b with
      | Value_body(v) ->
        Environment.add env x v;
        evaluate env (Some x) t
      | Var_body(x') ->
        let v = lookup env x' in
        Environment.add env x v;
        evaluate env (Some x) t
      | Input_body ->
        failwith "TODO"
      | Appl_body(x', x'') ->
        begin
          match lookup env x' with
          | Value_function(f) ->
            evaluate env (Some x) @@ fun_wire f x'' x @ t
          | r -> raise (Evaluation_failure
                          (Printf.sprintf
                             "cannot apply %s as it contains non-function %s"
                             (show_var x') (show_value r)))
        end
      | Conditional_body(x',e1,e2) ->
        let v = lookup env x' in
        let e_target =
          match v with
          | Value_bool b -> if b then e1 else e2
          | _ ->
            raise (Evaluation_failure
                     (Printf.sprintf
                        "cannot condition on non-boolean value %s"
                        (show_value v)))
        in
        evaluate env (Some x) @@ cond_wire x e_target @ t
      | Binary_operation_body(x1,op,x2) ->
        let v1 = lookup env x1 in
        let v2 = lookup env x2 in
        let result =
          begin
            match v1,op,v2 with
            | (Value_int(n1),Binary_operator_plus,Value_int(n2)) ->
              Value_int(n1+n2)
            | (Value_int(n1),Binary_operator_minus,Value_int(n2)) ->
              Value_int(n1-n2)
            | (Value_int(n1),Binary_operator_less_than,Value_int(n2)) ->
              Value_bool (n1 < n2)
            | ( Value_int(n1),
                Binary_operator_less_than_or_equal_to,
                Value_int(n2)
              ) ->
              Value_bool (n1 <= n2)
            | (Value_int(n1),Binary_operator_equal_to,Value_int(n2)) ->
              Value_bool (n1 = n2)
            | (Value_bool(b1),Binary_operator_equal_to,Value_bool(b2)) ->
              Value_bool (b1 = b2)
            | (Value_bool(b1),Binary_operator_and,Value_bool(b2)) ->
              Value_bool (b1 && b2)
            | (Value_bool(b1),Binary_operator_or,Value_bool(b2)) ->
              Value_bool (b1 || b2)
            | v1,op,v2 ->
              raise @@ Evaluation_failure(
                Printf.sprintf "Cannot complete binary operation: (%s) %s (%s)"
                  (show_value v1) (show_binary_operator op) (show_value v2))
          end
        in
        Environment.add env x result;
        evaluate env (Some x) t
    end
;;

let eval e =
  let env = Environment.create(20) in
  let repl_fn = repl_fn_for e (Freshening_stack []) Var_set.empty in
  let Expr(cls) = var_replace_expr repl_fn e in
  evaluate env None cls
;;
