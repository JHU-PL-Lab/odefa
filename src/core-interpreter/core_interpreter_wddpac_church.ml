open Batteries;;
open Jhupllib;;

open Core_ast;;
(* open Core_ast_pp;; *)

open Unbounded_context_stack_church;;
open Wddpac_graph;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

exception Evaluation_failure of string;;

let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last body in x
;;

let rec initialize_graph cls graph ctx =
  match cls with
  | [] -> ()
  | (Clause(x, cl2) as cl) :: tl ->
    begin
      match cl2 with
      | Value_body(Value_function(Function_value(x', Expr(e))) as f) ->

        Wddpac_graph.add_edge (x', Start_clause(Some(x')), ctx) graph;
        initialize_graph e graph (Some(x'));

        Wddpac_graph.add_edge (x, Function(x', rv e, f), ctx) graph;
        initialize_graph tl graph ctx

      | Conditional_body(xc,p,Function_value(x1, Expr(e1)),Function_value(x2, Expr(e2))) ->

        Wddpac_graph.add_edge (x1, Start_clause(Some(x1)), ctx) graph;
        initialize_graph e1 graph (Some(x1));

        Wddpac_graph.add_edge (x2, Start_clause(Some(x2)), ctx) graph;
        initialize_graph e2 graph (Some(x2));

        Wddpac_graph.add_edge (x, Conditional_clause(xc,p,x1,rv e1, x2, rv e2), ctx) graph;
        initialize_graph tl graph ctx
      | Value_body(Value_uint(v)) ->
        if v >= 0 then (
        Wddpac_graph.add_edge (x, Unannotated_clause(cl), ctx) graph;
        initialize_graph tl graph ctx)
        else raise @@ Utils.Invariant_failure "Unsigned int cannot be negative"
      | _ ->
        Wddpac_graph.add_edge (x, Unannotated_clause(cl), ctx) graph;
        initialize_graph tl graph ctx
    end
;;

let rec matches v p =
  match v,p with
  | _,Any_pattern -> true
  | Return_function _,Fun_pattern
  | Return_uint _,UInt_pattern
  | Return_int _,Int_pattern ->
    true
  | Return_bool actual_boolean,Bool_pattern pattern_boolean ->
    actual_boolean = pattern_boolean
  | _ -> false
;;

let rec lookup_ctx x0 context_stack =
  match Unbounded_Stack.top context_stack with
  | Some(v,x) -> if x0 = v then x else lookup_ctx x0 (Unbounded_Stack.pop context_stack)
  | None -> raise @@ Utils.Invariant_failure ("Lookup_ctx error in context stack" )
;;

let rec align_ctx loc context_stack =
  match Unbounded_Stack.top context_stack with
  | Some(v,_) -> if v = loc then context_stack else align_ctx loc (Unbounded_Stack.pop context_stack)
  | None -> raise @@ Utils.Invariant_failure ("Align ctx error in context stack" )
;;

let rec lookup graph var context_stack call_by_need =
  (* Align *)
  let Graph_node(node, loc) = Wddpac_graph.lookup var graph in

  begin
    match node with
    | Unannotated_clause(Clause(x, cl)) ->
      if x <> var then (
        raise @@ Utils.Invariant_failure ("Found no definitions for variable from exit clause 1" )
      )
      else
        begin
          match cl with
          | Var_body(x') ->
            (* print_endline "Alias";  *)
            lookup graph x' context_stack call_by_need
          | Value_body(Value_int(v)) ->
            Return_int(v)
          | Value_body(Value_uint(v)) ->
            let context_stack =
            begin
              match loc with
              | None -> Unbounded_Stack.empty
              | Some(loc) -> align_ctx (Stack_var(loc)) context_stack
            end in
            if v = 0 then Return_uint_zero(context_stack) else Return_uint(v, context_stack)
          | Value_body(Value_bool(v)) ->
            Return_bool(v)
          | Appl_body(xf, xv) ->
            (* print_endline "Lookup function"; *)
            let fn = lookup graph xf context_stack call_by_need in

            begin
              match fn with
              | Return_function(fn_ctx, rx, _, cfn_stack) ->
                  if call_by_need then lookup graph rx (Unbounded_Stack.push ((Stack_var(fn_ctx)), (ref (Context_var_table(Stack_var(xv), context_stack)))) cfn_stack) call_by_need
                  else (
                    let v = lookup graph xv context_stack call_by_need in
                    lookup graph rx (Unbounded_Stack.push ((Stack_var(fn_ctx)), (ref (Return_type(v)))) cfn_stack) call_by_need
                  )
              | _ -> raise @@ Utils.Invariant_failure "Found incorrect definitions for function 2"
            end
          | Binary_operation_body(x1,Binary_operator_plus,x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_int(n1 + n2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only add integers"
            end
          | Binary_operation_body(x1,Binary_operator_uint_plus,x2) ->
            let empty_ctx_stack = Unbounded_Stack.empty in
            let push_ctx_stack_1 = Unbounded_Stack.push ((Uint_var(M_add)), (ref (Context_var_table(Stack_var(x1), context_stack)))) empty_ctx_stack in
            let push_ctx_stack_2 = Unbounded_Stack.push ((Uint_var(N_add)), (ref (Context_var_table(Stack_var(x2), context_stack)))) push_ctx_stack_1 in
            Return_uint_add(push_ctx_stack_2)

          | Binary_operation_body(x1,Binary_operator_int_minus,x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_int(n1 - n2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only subtract integers"
            end
          | Binary_operation_body(_,Binary_operator_uint_minus,_) ->
              raise @@ Evaluation_failure "Uint minus not implemented"
          | Binary_operation_body(x1,Binary_operator_equal_to,x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_bool(n1 == n2)
              | Return_bool(b1), Return_bool(b2) -> Return_bool(b1 == b2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only equate integers"
            end
          | Binary_operation_body(x1,Binary_operator_uint_equal_to,x2) ->

              let v1 = lookup graph x1 context_stack call_by_need in
              let v1 = unwrap (update_uint_ctx_stack v1 (Return_type(Return_uint_final_zero))) graph call_by_need in
              let v2 = lookup graph x2 context_stack call_by_need in
              let v2 = unwrap (update_uint_ctx_stack v2 (Return_type(Return_uint_final_zero))) graph call_by_need in
              Return_bool((compare_vars 0 v1 0 v2 graph call_by_need) == 0)

          | Binary_operation_body(x1,Binary_operator_int_less_than,x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_bool(n1 < n2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only compare less than on integers"
            end
          | Binary_operation_body(x1,Binary_operator_uint_less_than,x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v1 = unwrap (update_uint_ctx_stack v1 (Return_type(Return_uint_final_zero))) graph call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            let v2 = unwrap (update_uint_ctx_stack v2 (Return_type(Return_uint_final_zero))) graph call_by_need in
            Return_bool((compare_vars 0 v1 0 v2 graph call_by_need) < 0)

          | Binary_operation_body(x1, Binary_operator_int_less_than_or_equal_to, x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_bool(n1 <= n2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only compare less than or equal to on integers"
            end
          | Binary_operation_body(x1, Binary_operator_uint_less_than_or_equal_to, x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v1 = unwrap (update_uint_ctx_stack v1 (Return_type(Return_uint_final_zero))) graph call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            let v2 = unwrap (update_uint_ctx_stack v2 (Return_type(Return_uint_final_zero))) graph call_by_need in
            Return_bool((compare_vars 0 v1 0 v2 graph call_by_need) <= 0)
          | Binary_operation_body(x1,Binary_operator_bool_and,x2) ->
            begin
              match lookup graph x1 context_stack call_by_need with
              | Return_bool(b) ->
                if not b then Return_bool(false) else
                  begin
                    match lookup graph x2 context_stack call_by_need with
                    | Return_bool(b) -> Return_bool(b)
                    | _ ->
                     raise @@ Evaluation_failure "Can only and booleans"
                  end
              | _ ->
               raise @@ Evaluation_failure "Can only and booleans"
            end
          | Binary_operation_body(x1,Binary_operator_bool_or,x2) ->
            begin
              match lookup graph x1 context_stack call_by_need with
              | Return_bool(b) ->
                if b then Return_bool(true) else
                  begin
                    match lookup graph x2 context_stack call_by_need with
                    | Return_bool(b) -> Return_bool(b)
                    | _ ->
                     raise @@ Evaluation_failure "Can only or booleans"
                  end
              | _ ->
               raise @@ Evaluation_failure "Can only or booleans"
            end
          | Unary_operation_body(op,x1) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            begin
              match op, v1 with
              | Unary_operator_bool_not, Return_bool(b1) -> Return_bool(not b1)
              | _,_ ->
               raise @@ Evaluation_failure "Incorrect unary operation"
            end
          | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause"
        end
    | Start_clause(None) ->
      raise @@ Utils.Invariant_failure ("Found no definitions for variable 4")
    | Start_clause(Some(x0)) ->
      (* print_endline "Start Clause"; *)
      local_lookup graph (Stack_var(x0)) context_stack call_by_need

    | Function(x0,rx, f) ->
      let context_stack =
      begin
        match loc with
        | None -> Unbounded_Stack.empty
        | Some(loc) -> align_ctx (Stack_var(loc)) context_stack
      end in
      Return_function(x0, rx, f, context_stack)

    | Conditional_clause(xc, p, x1,rx1,x2,rx2) ->
      let v = lookup graph xc context_stack call_by_need in
      let fn_ctx, rx = if matches v p then (x1,rx1) else (x2,rx2) in
      lookup graph rx (Unbounded_Stack.push ((Stack_var(fn_ctx)), (ref (Return_type(v)))) context_stack) call_by_need
  end
and compare_vars v1 x1 v2 x2 graph call_by_need =
  match x1, x2 with
  | Return_uint_final_zero, Return_uint_final_zero -> compare v1 v2
  | Return_uint_final_zero, Return_uint(v3, ctx_stack) ->
    if v2 > v1 then -1
    else (compare_vars v1 x1 (v2 + v3) (lookup_uint graph X_add ctx_stack call_by_need) graph call_by_need)
  | Return_uint(v3, ctx_stack), Return_uint_final_zero ->
    if v1 > v2 then 1 else (compare_vars (v1+v3) (lookup_uint graph X_add ctx_stack call_by_need) v2 x2  graph call_by_need)

  | Return_uint(v3, ctx_stack1), Return_uint(v4, ctx_stack2) ->
    (compare_vars (v1+v3) (lookup_uint graph X_add ctx_stack1 call_by_need) (v2+v4) (lookup_uint graph X_add ctx_stack2 call_by_need)  graph call_by_need)
  | _, _ -> raise @@ Evaluation_failure "Error in compare vars"

and local_lookup graph var context_stack call_by_need =
  let x = lookup_ctx var context_stack in
  begin
    match !x with
    | Return_type(v) -> v
    | Context_var_table(Stack_var(xv), ctx_stack) ->
      let v = lookup graph xv ctx_stack call_by_need in
      x := Return_type(v);
      v
    | Context_var_table(Uint_var(xv), ctx_stack) ->
      let v = lookup_uint graph xv ctx_stack call_by_need in
      x := Return_type(v);
      v
  end

and unwrap v graph call_by_need =
  match v with
  | Return_uint_final_zero ->
    Return_uint_final_zero
  | Return_uint_add (context_stack) ->
    lookup_uint graph M_add context_stack call_by_need
  | Return_uint_zero (context_stack) ->
    lookup_uint graph X_add context_stack call_by_need
  | Return_uint(_,_) ->
    v
  | _ -> raise @@ Evaluation_failure "Error in unwrap fn"
and update_uint_ctx_stack v var =

  begin
    match v with
    | Return_uint(v, context_stack) ->
      let context_stack = Unbounded_Stack.push ((Uint_var(X_add)), (ref var)) context_stack in
      Return_uint(v, context_stack)

    | Return_uint_zero (context_stack) ->
      let context_stack = Unbounded_Stack.push ((Uint_var(X_add)), (ref var)) context_stack in
      Return_uint_zero(context_stack)

    | Return_uint_add (context_stack) ->
      let context_stack = Unbounded_Stack.push ((Uint_var(X_add)), (ref var)) context_stack in
      Return_uint_add(context_stack)
    | _ -> raise @@ Evaluation_failure "Eval Return_uint_zero not implemented"
  end

and lookup_uint graph var context_stack call_by_need =
  let v = local_lookup graph (Uint_var(var)) context_stack call_by_need in
  match var with
  | X_add ->
    unwrap v graph call_by_need

  | M_add ->
    unwrap (update_uint_ctx_stack v (Context_var_table(Uint_var(N_add), context_stack))) graph call_by_need

  | N_add ->
    unwrap (update_uint_ctx_stack v (Context_var_table(Uint_var(X_add), context_stack))) graph call_by_need
;;

let rec evaluate_uint v graph call_by_need =
  match v with
  | Return_uint_final_zero -> 0
  | Return_uint_zero (context_stack) ->
    evaluate_uint (lookup_uint graph X_add context_stack call_by_need) graph call_by_need
  | Return_uint(v, context_stack) ->
     v + (evaluate_uint (lookup_uint graph X_add context_stack call_by_need) graph call_by_need)
  | _ -> raise @@ Evaluation_failure "Error in evaluate fn"
;;

let rec substitute_return v graph env call_by_need =
  match v with
  | Return_bool(v) -> Value_bool(v)
  | Return_int(v) -> Value_int(v)
  | Return_function(_, _,f, context_stack) ->
    substitute f graph context_stack env call_by_need

  | Return_uint(_, _)
  | Return_uint_zero (_)
  | Return_uint_add (_) ->
    Value_uint(evaluate_uint (unwrap (update_uint_ctx_stack v (Return_type(Return_uint_final_zero))) graph call_by_need) graph call_by_need)
  | _ -> raise @@ Utils.Invariant_failure "Not implemented substitute"
and substitute v graph context_stack env call_by_need =
  match v with
  | Value_function(Function_value(x, Expr(e))) ->
    Var_hashtbl.add env x "0";
    Value_function(Function_value(x, Expr(substitute_expr e graph context_stack env call_by_need)))
  | _ -> v
and substitute_expr e graph context_stack env call_by_need =
  match e with
  | Clause(x, Value_body(v)) :: tl ->

    let v = substitute v graph context_stack env call_by_need in
    Var_hashtbl.add env x "0";
    Clause(x, Value_body(v)) :: substitute_expr tl graph context_stack env call_by_need

  | (Clause(x, Unary_operation_body(_,x')) as hd) :: tl
  | (Clause(x, Var_body(x')) as hd) :: tl ->

      let assignments = process_vars (x' :: []) graph context_stack env call_by_need in
      Var_hashtbl.add env x "0";
      (assignments @ [hd]) @ substitute_expr tl graph context_stack env call_by_need
  | (Clause(x, Binary_operation_body(x',_,x'')) as hd) :: tl
  | (Clause(x, Appl_body(x', x'')) as hd) :: tl ->

      let assignments = process_vars (x' :: x'' :: []) graph context_stack env call_by_need in
      Var_hashtbl.add env x "0";
      (assignments @ [hd]) @ substitute_expr tl graph context_stack env call_by_need

  | Clause(x, Conditional_body(x',op,f1,f2)) :: tl ->
    let assignments = process_vars (x' :: []) graph context_stack env call_by_need in
    let f1 = substitute (Value_function(f1)) graph context_stack env call_by_need in
    let f2 = substitute (Value_function(f2)) graph context_stack env call_by_need in
    Var_hashtbl.add env x "0";
    begin
      match f1,f2 with
      | Value_function(f1), Value_function(f2) ->
        (assignments @ ([Clause(x, Conditional_body(x',op,f1,f2))])) @ substitute_expr tl graph context_stack env call_by_need
      | _, _ -> raise @@ Utils.Invariant_failure "Incorrect substitution of function"
    end
  | _ -> []
and process_vars vars graph context_stack env call_by_need =
  List.enum vars
  |> Enum.filter (fun x -> not (Var_hashtbl.mem env x))
  |> Enum.map (fun x ->
      let v = lookup graph x context_stack call_by_need in
      Var_hashtbl.add env x "0";
      Clause(x, Value_body(substitute_return v graph (Var_hashtbl.create 10) call_by_need)))
  |> List.of_enum
;;

let eval (Expr(cls)) call_by_need : Core_ast.var * value Core_interpreter.Environment.t =
  let context_stack = Unbounded_Stack.empty in
  let graph = Wddpac_graph.empty in
  initialize_graph cls graph None;
  let rx = rv cls in
  let v = lookup graph rx context_stack call_by_need in
  let v = substitute_return v graph (Var_hashtbl.create 10) call_by_need in
  let env = Core_interpreter.Environment.create 10 in
  Core_interpreter.Environment.add env (rv cls) v;
  (rv cls), env
;;
