open Batteries;;
open Jhupllib;;

open Core_ast;;
(* open Core_ast_pp;; *)

open Unbounded_context_stack_my_uint;;
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
  | Some(v,_) -> if Some(v) = loc then context_stack else align_ctx loc (Unbounded_Stack.pop context_stack)
  | None -> if loc = None then context_stack else raise @@ Utils.Invariant_failure ("Align ctx error in context stack" )
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
            Return_uint(v, Cont_frame(Unbounded_Stack.empty))

          | Value_body(Value_bool(v)) ->
            Return_bool(v)
          | Appl_body(xf, xv) ->
            (* print_endline "Lookup function"; *)
            let fn = lookup graph xf context_stack call_by_need in

            begin
              match fn with
              | Return_function(fn_ctx, rx, _, cfn_stack) ->
                  if call_by_need then lookup graph rx (Unbounded_Stack.push (fn_ctx, (ref (Context_var_table(xv, context_stack)))) cfn_stack) call_by_need
                  else (
                    let v = lookup graph xv context_stack call_by_need in
                    lookup graph rx (Unbounded_Stack.push (fn_ctx, (ref (Return_type(v)))) cfn_stack) call_by_need
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

              let cont_stack = Unbounded_Stack.empty in
              let cont_stack = Unbounded_Stack.push (x2, context_stack) cont_stack in
              let cont_stack = Unbounded_Stack.push (x1, context_stack) cont_stack in
              Return_uint(0, Cont_frame(cont_stack))

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

            let curr_frame1 = [(x1, context_stack)] in
            let curr_frame2 = [(x2, context_stack)] in
            let res = compare_vars graph call_by_need None curr_frame1 0 0 [] None curr_frame2 0 0 [] in
            Return_bool(res == 0)

          | Binary_operation_body(x1,Binary_operator_int_less_than,x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_bool(n1 < n2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only compare less than on integers"
            end
          | Binary_operation_body(_,Binary_operator_uint_less_than,_) ->
            raise @@ Evaluation_failure "Comparison not implemented"
          | Binary_operation_body(x1, Binary_operator_int_less_than_or_equal_to, x2) ->
            let v1 = lookup graph x1 context_stack call_by_need in
            let v2 = lookup graph x2 context_stack call_by_need in
            begin
              match v1, v2 with
              | Return_int(n1), Return_int(n2) -> Return_bool(n1 <= n2)
              | _,_ ->
               raise @@ Evaluation_failure "Can only compare less than or equal to on integers"
            end
          | Binary_operation_body(_, Binary_operator_uint_less_than_or_equal_to, _) ->
            raise @@ Evaluation_failure "Comparison not implemented"
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
    | Start_clause(Some(var)) ->
      (* print_endline "Start Clause"; *)
      let x = lookup_ctx var context_stack in
      begin
        match !x with
        | Return_type(v) ->
          v
        | Context_var_table(xv, ctx_stack) ->
          let v = lookup graph xv ctx_stack call_by_need in
          x := Return_type(v);
          v
      end

    | Function(x0,rx, f) ->
      let context_stack = align_ctx loc context_stack in
      Return_function(x0, rx, f, context_stack)

    | Conditional_clause(xc, p, x1,rx1,x2,rx2) ->
      let v = lookup graph xc context_stack call_by_need in
      let fn_ctx, rx = if matches v p then (x1,rx1) else (x2,rx2) in
      lookup graph rx (Unbounded_Stack.push (fn_ctx, (ref (Return_type(v)))) context_stack) call_by_need
  end

and set_ref_option x v =
  match x with
  | None -> ()
  | Some(x) -> x := v

and cont_uint_comp graph var context_stack call_by_need local_var curr_frame uint_val local_val comp_stack =
    (* Align *)
    let Graph_node(node, _) = Wddpac_graph.lookup var graph in
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

              cont_uint_comp graph x' context_stack call_by_need local_var curr_frame uint_val local_val comp_stack

            | Value_body(Value_int(_)) ->

              raise @@ Utils.Invariant_failure ("Found integer instead of uint")

            | Value_body(Value_uint(v)) ->

              set_ref_option local_var (Return_type(Return_uint(v + uint_val - local_val, Cont_frame(curr_frame))));

              if v = 0 then
                continue_uint graph call_by_need local_var curr_frame uint_val local_val comp_stack

              else
              (v + uint_val, curr_frame, local_var, local_val, comp_stack)

            | Value_body(Value_bool(_)) ->

              raise @@ Utils.Invariant_failure ("Found bool instead of uint")

            | Appl_body(xf, xv) ->

              let fn = lookup graph xf context_stack call_by_need in

              begin
                match fn with
                | Return_function(fn_ctx, rx, _, cfn_stack) ->
                    if call_by_need then
                      cont_uint_comp graph rx (Unbounded_Stack.push (fn_ctx, (ref (Context_var_table(xv, context_stack)))) cfn_stack) call_by_need local_var curr_frame uint_val local_val comp_stack
                    else (
                      let v = lookup graph xv context_stack call_by_need in

                      cont_uint_comp graph rx (Unbounded_Stack.push (fn_ctx, (ref (Return_type(v)))) cfn_stack) call_by_need local_var curr_frame uint_val local_val comp_stack
                    )
                | _ -> raise @@ Utils.Invariant_failure "Found incorrect definitions for function 2"
              end
            | Binary_operation_body(x1,Binary_operator_uint_plus,x2) ->

              let curr_frame = Unbounded_Stack.push (x2, context_stack) curr_frame in

              set_ref_option local_var (Return_type(Return_uint(uint_val - local_val, Cont_frame(curr_frame))));

              cont_uint_comp graph x1 context_stack call_by_need local_var curr_frame uint_val local_val comp_stack

            | Binary_operation_body(_,Binary_operator_uint_minus,_) ->
              raise @@ Evaluation_failure "Uint minus not implemented"

            | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause or incorrectly typed expression"
          end
      | Start_clause(None) ->
        raise @@ Utils.Invariant_failure ("Found no definitions for variable 4")
      | Start_clause(Some(var)) ->
        (* print_endline "Start Clause"; *)
        let x = lookup_ctx var context_stack in
        let v =
          begin
            match !x with
            | Return_type(v) ->
              v
            | Context_var_table(xv, ctx_stack) ->
              let v = lookup graph xv ctx_stack call_by_need in
              x := Return_type(v);
              v
          end in
          begin
            match v with
            | Return_uint(v, Cont_frame(frame)) ->

              if v = 0 then
                continue_uint graph call_by_need (Some(x)) frame uint_val uint_val (Unbounded_Stack.push (local_var, local_val, curr_frame) comp_stack)

              else

                (v + uint_val, frame, Some(x), uint_val, (Unbounded_Stack.push (local_var, local_val, curr_frame) comp_stack))

            | _ -> raise @@ Utils.Invariant_failure "Found incorrect type instead of uint"
          end

      | Function(_,_,_) ->
        raise @@ Utils.Invariant_failure "Found function instead of uint"

      | Conditional_clause(xc, p, x1,rx1,x2,rx2) ->
        let v = lookup graph xc context_stack call_by_need in
        let fn_ctx, rx = if matches v p then (x1,rx1) else (x2,rx2) in

        cont_uint_comp graph rx (Unbounded_Stack.push (fn_ctx, (ref (Return_type(v)))) context_stack) call_by_need local_var curr_frame uint_val local_val comp_stack

    end
and is_done frame stack =
  match frame, stack with
  | [], [] -> true
  | _,_ -> false
and compare_vars graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 local_var2 curr_frame2 uint_val2 local_val2 comp_stack2 =

  match is_done curr_frame1 comp_stack1, is_done curr_frame2 comp_stack2 with
  | true, true -> compare uint_val1 uint_val2
  | true, false ->
    if uint_val2 > uint_val1 then -1
    else (
      let (uint_val2, curr_frame2, local_var2, local_val2, comp_stack2) = continue_uint graph call_by_need local_var2 curr_frame2 uint_val2 local_val2 comp_stack2 in

      compare_vars graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 local_var2 curr_frame2 uint_val2 local_val2 comp_stack2
    )

  | false, true ->
    if uint_val1 > uint_val2 then 1
    else (
      let (uint_val1, curr_frame1, local_var1, local_val1, comp_stack1) = continue_uint graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 in

      compare_vars graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 local_var2 curr_frame2 uint_val2 local_val2 comp_stack2
    )

  | false, false ->

      if v1 < v2 then (
        let (uint_val1, curr_frame1, local_var1, local_val1, comp_stack1) = continue_uint graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 in
        compare_vars graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 local_var2 curr_frame2 uint_val2 local_val2 comp_stack2
      ) else (
        let (uint_val2, curr_frame2, local_var2, local_val2, comp_stack2) = continue_uint graph call_by_need local_var2 curr_frame2 uint_val2 local_val2 comp_stack2 in
        compare_vars graph call_by_need local_var1 curr_frame1 uint_val1 local_val1 comp_stack1 local_var2 curr_frame2 uint_val2 local_val2 comp_stack2
      )

and continue_uint graph call_by_need local_var curr_frame uint_val local_val comp_stack =
  match curr_frame with
  | [] ->
    begin
      match comp_stack with
      | [] ->
          (uint_val, [], None, 0, [])

      | (local_var, local_val, curr_frame) :: comp_stack ->

        continue_uint graph call_by_need local_var curr_frame uint_val local_val comp_stack

    end
  | (x, context_stack) :: t ->

    cont_uint_comp graph x context_stack call_by_need local_var t uint_val local_val comp_stack
;;

let rec unwrap_uint graph call_by_need local_var frame uint_val local_val comp_stack  =

  if is_done frame comp_stack then uint_val
  else (
    let (uint_val, frame, local_var, local_val, comp_stack) = continue_uint graph call_by_need local_var frame uint_val local_val comp_stack in

    unwrap_uint graph call_by_need local_var frame uint_val local_val comp_stack
  )
;;

let rec substitute_return v graph env call_by_need =
  match v with
  | Return_bool(v) -> Value_bool(v)
  | Return_int(v) -> Value_int(v)
  | Return_function(_, _,f, context_stack) ->
    substitute f graph context_stack env call_by_need
  | Return_uint(v, Cont_frame(frame)) ->
    Value_uint(unwrap_uint graph call_by_need None frame v 0 [])
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
