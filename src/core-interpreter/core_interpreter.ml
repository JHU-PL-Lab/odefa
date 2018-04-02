open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
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
let bound_vars_of_expr (Expr(cls)) =
  cls
  |> List.map (fun (Clause(x, _)) -> x)
  |> Var_set.of_list
;;

let rec var_replace_expr fn (Expr(cls)) =
  Expr(List.map (var_replace_clause fn) cls)

and var_replace_clause fn (Clause(x, b)) =
  Clause(fn x, var_replace_clause_body fn b)

and var_replace_clause_body fn r =
  match r with
  | Value_body(v) -> Value_body(var_replace_value fn v)
  | Var_body(x) -> Var_body(fn x)
  | Appl_body(x1, x2) -> Appl_body(fn x1, fn x2)
  | Conditional_body(x,p,f1,f2) ->
    Conditional_body(fn x, p, var_replace_function_value fn f1,
                     var_replace_function_value fn f2)
  | Projection_body(x,i) -> Projection_body(fn x, i)
  | Deref_body(x) -> Deref_body(fn x)
  | Update_body(x1,x2) -> Update_body(fn x1, fn x2)
  | Binary_operation_body(x1,op,x2) -> Binary_operation_body(fn x1, op, fn x2)
  | Unary_operation_body(op,x1) -> Unary_operation_body(op, fn x1)

and var_replace_value fn v =
  match v with
  | Value_record(Record_value(es)) ->
    Value_record(Record_value(Ident_map.map fn es))
  | Value_function(f) -> Value_function(var_replace_function_value fn f)
  | Value_ref(Ref_value(x)) -> Value_ref(Ref_value(fn x))
  | Value_int n -> Value_int n
  | Value_uint n ->
    if n >= 0 then Value_uint n else raise @@ Utils.Invariant_failure "Unsigned int cannot be negative"
  | Value_bool b -> Value_bool b
  | Value_string s -> Value_string s

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
    (* FIXME: this functionality is duplicated above in bound_vars_of_expr; this
       needs to be fixed upstream. *)
    clauses
    |> List.map (fun (Clause(x,_)) -> x)
    |> Var_set.of_list
    |> Var_set.union extra_bound
  in
  let repl_fn (Var(i, _) as x) =
    if Var_set.mem x bound_variables
    then Var(i, Some freshening_stack)
    else x
  in
  repl_fn
;;

let fresh_wire (Function_value(param_x, Expr(body))) arg_x call_site_x =
  (* Build the variable freshening function. *)
  let freshening_stack = freshening_stack_from_var call_site_x in
  let repl_fn =
    repl_fn_for body freshening_stack @@ Var_set.singleton param_x in
  (* Create the freshened, wired body. *)
  let freshened_body = List.map (var_replace_clause repl_fn) body in
  let head_clause = Clause(repl_fn param_x, Var_body(arg_x)) in
  let Clause(last_var,_) = List.last freshened_body in
  let tail_clause = Clause(call_site_x, Var_body(last_var)) in
  [head_clause] @ freshened_body @ [tail_clause]
;;

let rec matches env x p =
  let v = lookup env x in
  match v,p with
  | _,Any_pattern -> true
  | Value_record(Record_value(els)),Record_pattern(els') ->
    els'
    |> Ident_map.enum
    |> Enum.for_all
      (fun (i,p') ->
         try
           matches env (Ident_map.find i els) p'
         with
         | Not_found -> false
      )
  | Value_function(Function_value(_)),Fun_pattern
  | Value_ref(Ref_value(_)),Ref_pattern
  | Value_int _,Int_pattern
  | Value_uint _,UInt_pattern
  | Value_string _,String_pattern ->
    true
  | Value_bool actual_boolean,Bool_pattern pattern_boolean ->
    actual_boolean = pattern_boolean
  | _ -> false
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
      | Appl_body(x', x'') ->
        begin
          match lookup env x' with
          | Value_function(f) ->
            evaluate env (Some x) @@ fresh_wire f x'' x @ t
          | r -> raise (Evaluation_failure
                          (Printf.sprintf
                             "cannot apply %s as it contains non-function %s"
                             (show_var x') (show_value r)))
        end
      | Conditional_body(x',p,f1,f2) ->
        let f_target = if matches env x' p then f1 else f2 in
        evaluate env (Some x) @@ fresh_wire f_target x' x @ t
      | Projection_body(x', i) ->
        begin
          match lookup env x' with
          | Value_record(Record_value(els)) as r ->
            begin
              try
                let x'' = Ident_map.find i els in
                let v = lookup env x'' in
                Environment.add env x v;
                evaluate env (Some x) t
              with
              | Not_found ->
                raise @@ Evaluation_failure(
                  Printf.sprintf "cannot project %s from %s: not present"
                    (show_ident i) (show_value r))
            end
          | v ->
            raise @@ Evaluation_failure(
              Printf.sprintf "cannot project %s from non-record value %s"
                (show_ident i) (show_value v))
        end
      | Deref_body(x') ->
        let v = lookup env x' in
        begin
          match v with
          | Value_ref(Ref_value(x'')) ->
            let v' = lookup env x'' in
            Environment.add env x v';
            evaluate env (Some x) t
          | _ -> raise @@ Evaluation_failure(
              Printf.sprintf "cannot dereference %s as it contains non-reference %s"
                (show_var x') (show_value v))
        end
      | Update_body(x', x'') ->
        let v = lookup env x' in
        let v' = lookup env x'' in
        begin
          match v with
          | Value_ref(Ref_value(x'')) ->
            Environment.replace env x'' v';
            evaluate env (Some x)
              ((Clause(x,
                       Value_body(Value_record(Record_value(Ident_map.empty)))))::t)
          | _ -> raise @@ Evaluation_failure(
              Printf.sprintf "cannot update %s as it contains non-reference %s"
                (show_var x') (show_value v))
        end
      | Binary_operation_body(x1,op,x2) ->
        let v1 = lookup env x1 in
        let v2 = lookup env x2 in
        let result =
          begin
            match v1,op,v2 with
            | (Value_int(n1),Binary_operator_plus,Value_int(n2)) ->
              Value_int(n1+n2)
            | (Value_uint(n1),Binary_operator_uint_plus,Value_uint(n2)) ->
              Value_uint(n1+n2)
            | (Value_int(n1),Binary_operator_int_minus,Value_int(n2)) ->
              Value_int(n1-n2)
            | (Value_uint(n1),Binary_operator_uint_minus,Value_uint(n2)) ->
              if n1 - n2 <= 0 then Value_uint(0) else Value_uint(n1-n2)
            | (Value_uint(n1),Binary_operator_uint_less_than,Value_uint(n2))
            | (Value_int(n1),Binary_operator_int_less_than,Value_int(n2)) ->
              Value_bool (n1 < n2)
            | ( Value_uint(n1)
              , Binary_operator_uint_less_than_or_equal_to
              , Value_uint(n2)
              )
            | ( Value_int(n1)
              , Binary_operator_int_less_than_or_equal_to
              , Value_int(n2)
              ) ->
              Value_bool (n1 <= n2)
            | (Value_uint(n1),Binary_operator_uint_equal_to,Value_uint(n2))
            | (Value_int(n1),Binary_operator_equal_to,Value_int(n2)) ->
              Value_bool (n1 = n2)
            | (Value_bool(b1),Binary_operator_equal_to,Value_bool(b2)) ->
              Value_bool (b1 = b2)
            | (Value_bool(b1),Binary_operator_bool_and,Value_bool(b2)) ->
              Value_bool (b1 && b2)
            | (Value_bool(b1),Binary_operator_bool_or,Value_bool(b2)) ->
              Value_bool (b1 || b2)
            | (Value_string(s1),Binary_operator_plus,Value_string(s2)) ->
              Value_string(s1 ^ s2)
            | (Value_string(s1),Binary_operator_equal_to,Value_string(s2)) ->
              Value_bool(s1 = s2)
            | (Value_string(s),Binary_operator_index,Value_int(i)) ->
              if i < String.length(s) then
                Value_string (String.make 1 (String.get s i))
              else
                Value_string ""
            | v1,op,v2 ->
              raise @@ Evaluation_failure(
                Printf.sprintf "Cannot complete binary operation: (%s) %s (%s)"
                  (show_value v1) (show_binary_operator op) (show_value v2))
          end
        in
        Environment.add env x result;
        evaluate env (Some x) t
      | Unary_operation_body(op,x1) ->
        let v1 = lookup env x1 in
        let result =
          begin
            match op,v1 with
            | (Unary_operator_bool_not,Value_bool(b1)) ->
              Value_bool (not b1)
            | (Unary_operator_bool_coin_flip,_) ->
              Value_bool (Random.bool ())
            | op,v1 ->
              raise @@ Evaluation_failure(
                Printf.sprintf "Cannot complete unary operation: %s (%s)"
                  (show_unary_operator op) (show_value v1))
          end
        in
        Environment.add env x result;
        evaluate env (Some x) t
    end
;;

let eval (Expr(cls)) =
  Random.self_init ();
  let env = Environment.create(20) in
  let repl_fn = repl_fn_for cls (Freshening_stack []) Var_set.empty in
  let cls' = List.map (var_replace_clause repl_fn) cls in
  evaluate env None cls'
;;
