open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Map.Make(Var);;
module Local_list = Var_hashtbl;;

type environment_value =
  | Environ_int of int
  | Environ_uint of int
  | Environ_bool of bool
  | Environ_function of function_value * (environment_value Environment.t)
  | Environ_string of string
  | Environ_record of record_value * (environment_value Environment.t)
;;

exception Evaluation_failure of string;;

let rec matches v p =
  match v,p with
  | _,Any_pattern -> true
  | Environ_record(Record_value(els), env),Record_pattern(els') ->
    els'
    |> Ident_map.enum
    |> Enum.for_all
      (fun (i,p') ->
         try
           matches (Environment.find (Ident_map.find i els) env) p'
         with
         | Not_found -> false
      )
  (* | Environ_ref(Ref_value(_)),Ref_pattern *)
  | Environ_function(_,_),Fun_pattern
  | Environ_int(_),Int_pattern
  | Environ_uint(_),UInt_pattern
  | Environ_string _,String_pattern ->
    true
  | Environ_bool actual_boolean,Bool_pattern pattern_boolean ->
    actual_boolean = pattern_boolean
  | _ -> false
;;

let get_value environ_value =
  match environ_value with
  | Environ_int(v) -> Value_int(v)
  | Environ_uint(v) -> Value_uint(v)
  | Environ_bool(v) -> Value_bool(v)
  | Environ_function(v,_) -> Value_function(v)
  | Environ_string(v) -> Value_string(v)
  | Environ_record(v, _) -> Value_record(v)
;;

let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last body in x
;;

let rec evaluate env cls =

  match cls with
  | [] -> env
  | (Clause(x, b)):: t ->
    begin
      match b with
      | Value_body(Value_int(v)) ->
        evaluate (Environment.add x (Environ_int(v)) env) t
      | Value_body(Value_bool(v)) ->
        evaluate (Environment.add x (Environ_bool(v)) env) t
      | Value_body(Value_function(v)) ->
        evaluate (Environment.add x (Environ_function(v,env)) env) t
      | Value_body(Value_string(v)) ->
        evaluate (Environment.add x (Environ_string(v)) env) t
      | Value_body(Value_uint(v)) ->
        if v >= 0 then evaluate (Environment.add x (Environ_uint(v)) env) t else raise @@ Utils.Invariant_failure "Unsigned int cannot be negative"
      | Value_body(Value_record(v)) ->
        evaluate (Environment.add x (Environ_record(v,env)) env) t
      (* | Value_body(Value_ref(v)) -> *)
        (* evaluate (Environment.add x (Environ_ref(v, env)) env) t *)
      | Var_body(x') ->
        let v = Environment.find x' env in
        evaluate (Environment.add x v env) t
      | Appl_body(x', x'') ->
        begin
          match Environment.find x' env with
          | Environ_function(Function_value(x''',Expr(cls2)), env2) ->
            let v = Environment.find x'' env in
            let env2 = evaluate (Environment.add x''' v env2) cls2 in
            evaluate (Environment.add x (Environment.find (rv cls2) env2) env) t

          | r -> raise (Evaluation_failure
                          (Printf.sprintf
                             "cannot apply %s as it contains non-function %s"
                             (show_var x') (show_value (get_value r))))
        end
      | Conditional_body(x',p,f1,f2) ->
        let v = Environment.find x' env in
        let Function_value(x'',Expr(cls2)) = if matches v p then f1 else f2 in
        let env2 = evaluate (Environment.add x'' v env) cls2 in
        evaluate (Environment.add x (Environment.find (rv cls2) env2) env) t
      | Projection_body(x', i) ->
        begin
          match Environment.find x' env with
          | Environ_record(Record_value(els), env2) as r ->
            begin
              try
                let x'' = Ident_map.find i els in
                let v = Environment.find x'' env2 in
                evaluate (Environment.add x v env) t
              with
              | Not_found ->
                raise @@ Evaluation_failure(
                  Printf.sprintf "cannot project %s from %s: not present"
                    (show_ident i) (show_value (get_value r)))
            end
          | v ->
            raise @@ Evaluation_failure(
              Printf.sprintf "cannot project %s from non-record value %s"
                (show_ident i) (show_value (get_value v)))
        end
      | Binary_operation_body(x1,op,x2) ->
        let v1 = Environment.find x1 env in
        let v2 = Environment.find x2 env in
        let result =
          begin
            match v1,op,v2 with
            | (Environ_int(n1),Binary_operator_plus,Environ_int(n2)) ->
              Environ_int(n1+n2)
            | (Environ_uint(n1),Binary_operator_uint_plus,Environ_uint(n2)) ->
              Environ_uint(n1+n2)
            | (Environ_int(n1),Binary_operator_int_minus,Environ_int(n2)) ->
              Environ_int(n1-n2)
            | (Environ_uint(n1),Binary_operator_uint_minus,Environ_uint(n2)) ->
              if n1 - n2 <= 0 then Environ_uint(0) else Environ_uint(n1-n2)
            | (Environ_uint(n1),Binary_operator_uint_less_than,Environ_uint(n2))
            | (Environ_int(n1),Binary_operator_int_less_than,Environ_int(n2)) ->
              Environ_bool (n1 < n2)
            | ( Environ_uint(n1)
              , Binary_operator_uint_less_than_or_equal_to
              , Environ_uint(n2)
              )
            | ( Environ_int(n1)
              , Binary_operator_int_less_than_or_equal_to
              , Environ_int(n2)
              ) ->
              Environ_bool (n1 <= n2)
            | (Environ_uint(n1),Binary_operator_uint_equal_to,Environ_uint(n2))
            | (Environ_int(n1),Binary_operator_equal_to,Environ_int(n2)) ->
              Environ_bool (n1 = n2)
            | (Environ_bool(b1),Binary_operator_equal_to,Environ_bool(b2)) ->
              Environ_bool (b1 = b2)
            | (Environ_bool(b1),Binary_operator_bool_and,Environ_bool(b2)) ->
              Environ_bool (b1 && b2)
            | (Environ_bool(b1),Binary_operator_bool_or,Environ_bool(b2)) ->
              Environ_bool (b1 || b2)
            | (Environ_string(s1),Binary_operator_plus,Environ_string(s2)) ->
              Environ_string(s1 ^ s2)
            | (Environ_string(s1),Binary_operator_equal_to,Environ_string(s2)) ->
              Environ_bool(s1 = s2)
            | (Environ_string(s),Binary_operator_index,Environ_int(i)) ->
              if i < String.length(s) then
                Environ_string (String.make 1 (String.get s i))
              else
                Environ_string ""
            | v1,op,v2 ->
              raise @@ Evaluation_failure(
                Printf.sprintf "Cannot complete binary operation: (%s) %s (%s)"
                  (show_value (get_value v1)) (show_binary_operator op) (show_value (get_value v2)))
          end
        in
        evaluate (Environment.add x result env) t
      | Unary_operation_body(op,x1) ->
        let v1 = Environment.find x1 env in
        let result =
          begin
            match op,v1 with
            | (Unary_operator_bool_not,Environ_bool(b1)) ->
              Environ_bool (not b1)
            | (Unary_operator_bool_coin_flip,_) ->
              Environ_bool (Random.bool ())
            | op,v1 ->
              raise @@ Evaluation_failure(
                Printf.sprintf "Cannot complete unary operation: %s (%s)"
                  (show_unary_operator op) (show_value (get_value v1)))
          end
        in
        evaluate (Environment.add x result env) t
      | _ ->
        raise @@ Evaluation_failure "Unimplemented features"
    end
;;

let rec subsitute_value environ_value =
  match environ_value with
  | Environ_int(v) -> Value_int(v)
  | Environ_uint(v) -> Value_uint(v)
  | Environ_bool(v) -> Value_bool(v)
  | Environ_function(v,env) -> substitute (Value_function(v)) (Local_list.create 10) env
  | Environ_string(v) -> Value_string(v)
  | Environ_record(Record_value(els) as v, env) ->
   (* TODO: replace variables in record *)
    els
      |> Ident_map.enum
      |> Enum.map
        (fun (i,v) ->
           (i, subsitute_value (Environment.find v env))
        )
      |> Ident_map.of_enum
      |> fun _ -> ();
    Value_record(v)
and substitute v env env2 =
  match v with
  | Value_function(Function_value(x, Expr(e))) ->
    Local_list.add env x "0";
    Value_function(Function_value(x, Expr(substitute_expr e env env2)))
  | _ -> v
and substitute_expr e env env2 =
  match e with
  | Clause(x, Value_body(v)) :: tl ->

    let v = substitute v env env2 in
    Local_list.add env x "0";
    Clause(x, Value_body(v)) :: substitute_expr tl env env2

  | (Clause(x, Unary_operation_body(_,x')) as hd) :: tl
  | (Clause(x, Var_body(x')) as hd) :: tl ->

      let assignments = process_vars (x' :: []) env env2 in
      Local_list.add env x "0";
      (assignments @ [hd]) @ substitute_expr tl env env2
  | (Clause(x, Binary_operation_body(x',_,x'')) as hd) :: tl
  | (Clause(x, Appl_body(x', x'')) as hd) :: tl ->

      let assignments = process_vars (x' :: x'' :: []) env env2 in
      Local_list.add env x "0";
      (assignments @ [hd]) @ substitute_expr tl env env2

  | Clause(x, Conditional_body(x',op,f1,f2)) :: tl ->
    let assignments = process_vars (x' :: []) env env2  in
    let f1 = substitute (Value_function(f1)) env env2  in
    let f2 = substitute (Value_function(f2)) env env2  in
    Local_list.add env x "0";
    begin
      match f1,f2 with
      | Value_function(f1), Value_function(f2) ->
        (assignments @ ([Clause(x, Conditional_body(x',op,f1,f2))])) @ substitute_expr tl env env2
      | _, _ -> raise @@ Utils.Invariant_failure "Incorrect substitution of function"
    end
  | _ -> []
and process_vars vars env env2 =
  List.enum vars
  |> Enum.filter (fun x -> not (Local_list.mem env x))
  |> Enum.map (fun x ->
      let environ_value = Environment.find x env2 in
      Local_list.add env x "0";
      Clause(x, Value_body(subsitute_value environ_value)))
  |> List.of_enum
;;

let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t =
  Random.self_init ();
  let env = evaluate (Environment.empty) cls in
  let v = subsitute_value (Environment.find (rv cls) env) in

  (* HACK: This is only here to get the types right to work with the original
  interpreter *)
  let env = Core_interpreter.Environment.create 10 in
  Core_interpreter.Environment.add env (rv cls) v;
  (rv cls), env
;;
