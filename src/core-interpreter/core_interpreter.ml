open Batteries;;
open Jhupllib;;

open Core_ast;;

open Unbounded_context_stack;;
open Wddpac_graph;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

type evaluation_environment = string Environment.t;;

exception Evaluation_failure of string;;

let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last body in x
;;

let rec initialize_graph cls graph ctx idx = 
  match cls with
  | [] -> idx
  | (Clause(x, cl2) as cl) :: tl ->
    begin
      match cl2 with
      | Value_body(Value_function(Function_value(x', Expr(e)))) -> 

        Wddpac_graph.add_edge (x', Start_clause(Some(x')), Some(x'), 0) graph;
        let _ = initialize_graph e graph (Some(x')) 1 in

        Wddpac_graph.add_edge (x, Unannotated_clause(cl), ctx, idx) graph;
        initialize_graph tl graph ctx (idx + 1);
      | Conditional_body(_,_,Function_value(x1, Expr(e1)),Function_value(x2, Expr(e2))) ->

        Wddpac_graph.add_edge (x1, Start_clause(Some(x1)), Some(x1), 0) graph;
        let _ = initialize_graph e1 graph (Some(x1)) 1 in

        Wddpac_graph.add_edge (x2, Start_clause(Some(x2)), Some(x2), 0) graph;
        let _ = initialize_graph e2 graph (Some(x2)) 1 in

        Wddpac_graph.add_edge (x, Unannotated_clause(cl), ctx, idx) graph;

        initialize_graph tl graph ctx (idx + 1);
      | _ -> 
        Wddpac_graph.add_edge (x, Unannotated_clause(cl), ctx, idx) graph;
        initialize_graph tl graph ctx (idx + 1);
    end
;;

let rec matches v p =
  match v,p with
  | _,Any_pattern -> true
  (* | Value_record(Record_value(els)),Record_pattern(els') ->
    els'
    |> Ident_map.enum
    |> Enum.for_all
      (fun (i,p') ->
         try
           matches (Ident_map.find i els) p'
         with
         | Not_found -> false
      ) *)
  | Value_function(Function_value(_)),Fun_pattern
  | Value_ref(Ref_value(_)),Ref_pattern
  | Value_string _,String_pattern
  | Value_int _,Int_pattern ->
    true
  | Value_bool actual_boolean,Bool_pattern pattern_boolean ->
    actual_boolean = pattern_boolean
  | _ -> false
;;

let rec lookup_ctx context_stack (ctx, index) (c,i) =
  if (c = ctx && i <= index) then context_stack
  else
    begin
      match Unbounded_Stack.top context_stack with
      | Some(Appl_context_var(_, _, loc, context_stack)) -> 
        lookup_ctx context_stack loc (c,i)
      | Some(Cond_context_var(_, loc)) -> 
        lookup_ctx (Unbounded_Stack.pop context_stack) loc (c,i)  
      | _ -> raise @@ Utils.Invariant_failure "Incorrect context stack2"
    end
;;

let rec lookup graph var curr_loc context_stack = 
  (* Align *)
  let Graph_node(node, loc) = Wddpac_graph.lookup var graph in
  let context_stack = lookup_ctx context_stack curr_loc loc in

  (* Process Graph Node *)
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
            lookup graph x' loc context_stack
            
          | Value_body(Value_function(_) as v) 
          | Value_body(Value_int(_) as v) 
          | Value_body(Value_bool(_) as v) -> 

            (v, loc, context_stack)

          | Appl_body(xf, xv) -> 
            (* print_endline "Lookup function"; *)
            let (fn, cfn, cfn_stack) = lookup graph xf loc context_stack in

            begin
              match fn with
              | Value_function(Function_value(fn_ctx, Expr(cls))) ->
                  let size = List.length cls in 
                  let rx = rv cls in

                  lookup graph rx (Some(fn_ctx), size) (Unbounded_Stack.push (Appl_context_var(xv, loc, cfn, cfn_stack)) context_stack)

              | _ -> raise @@ Utils.Invariant_failure "Found incorrect definitions for function 2"
            end
          | Conditional_body(xc,p,f1,f2) ->
            let (v, _, _) = lookup graph xc loc context_stack in
            let Function_value(fn_ctx, Expr(cls)) = if matches v p then f1 else f2 in
            let size = List.length cls in 
            let rx = rv cls in
            lookup graph rx (Some(fn_ctx), size) (Unbounded_Stack.push (Cond_context_var(xc, loc)) context_stack)

          | Binary_operation_body(x1,op,x2) ->
            let (v1, _, _) = lookup graph x1 loc context_stack in 
            let (v2, _, _) = lookup graph x2 loc context_stack in 
            begin
              match v1, op, v2 with
              | Value_int(n1), Binary_operator_plus, Value_int(n2) -> (Value_int(n1 + n2), loc, context_stack)
              | Value_int(n1), Binary_operator_int_minus, Value_int(n2) -> (Value_int(n1 - n2), loc, context_stack)
              | Value_int(n1), Binary_operator_equal_to, Value_int(n2) -> (Value_bool(n1 = n2), loc, context_stack)
              | Value_bool(b1), Binary_operator_bool_and, Value_bool(b2) -> (Value_bool(b1 && b2), loc, context_stack)
              | Value_bool(b1), Binary_operator_bool_or, Value_bool(b2) -> (Value_bool(b1 || b2), loc, context_stack)
              | _,_,_ -> 
               raise @@ Evaluation_failure "Incorrect binary operation"
            end
          | Unary_operation_body(op,x1) ->
            let (v1, _, _) = lookup graph x1 loc context_stack in 
            begin
              match op, v1 with
              | Unary_operator_bool_not, Value_bool(b1) -> (Value_bool(not b1), loc, context_stack)
              | _,_ -> 
               raise @@ Evaluation_failure "Incorrect unary operation"
            end

          | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause"
        end
    | Start_clause(None) -> 
      raise @@ Utils.Invariant_failure ("Found no definitions for variable 4")
    | Start_clause(Some(_)) ->
      (* print_endline "Start Clause"; *)
      begin
        match Unbounded_Stack.top context_stack with
        | Some(Appl_context_var(var, loc, _, _))
        | Some(Cond_context_var(var, loc)) -> 
          lookup graph var loc (Unbounded_Stack.pop context_stack)
        | _ -> raise @@ Utils.Invariant_failure "Incorrect context stack"
      end
  end 
;;

let rec substitute v cl graph context_stack env = 
  match v with
  | Value_function(Function_value(x, Expr(e))) ->
    Environment.add env x "0";
    Value_function(Function_value(x, Expr(substitute_expr e cl graph context_stack env)))
  | _ -> v
and substitute_expr e cl graph context_stack env =
  match e with 
  | Clause(x, Value_body(v)) :: tl ->

    let v = substitute v cl graph context_stack env in
    Environment.add env x "0";
    Clause(x, Value_body(v)) :: substitute_expr tl cl graph context_stack env

  | (Clause(x, Unary_operation_body(_,x')) as hd) :: tl
  | (Clause(x, Var_body(x')) as hd) :: tl -> 

      let assignments = process_vars (x' :: []) cl graph context_stack env in
      Environment.add env x "0";
      (assignments @ [hd]) @ substitute_expr tl cl graph context_stack env
  | (Clause(x, Binary_operation_body(x',_,x'')) as hd) :: tl 
  | (Clause(x, Appl_body(x', x'')) as hd) :: tl ->

      let assignments = process_vars (x' :: x'' :: [])  cl graph context_stack env in
      Environment.add env x "0";
      (assignments @ [hd]) @ substitute_expr tl cl graph context_stack env
   
  | Clause(x, Conditional_body(x',op,f1,f2)) :: tl -> 
    let assignments = process_vars (x' :: []) cl graph context_stack env in
    let f1 = substitute (Value_function(f1)) cl graph context_stack env in
    let f2 = substitute (Value_function(f2)) cl graph context_stack env in 
    Environment.add env x "0";
    begin
      match f1,f2 with
      | Value_function(f1), Value_function(f2) ->
        (assignments @ ([Clause(x, Conditional_body(x',op,f1,f2))])) @ substitute_expr tl cl graph context_stack env
      | _, _ -> raise @@ Utils.Invariant_failure "Incorrect substitution of function" 
    end
  | _ -> []
and process_vars vars cl graph context_stack env = 
  List.enum vars
  |> Enum.filter (fun x -> not (Environment.mem env x))
  |> Enum.map (fun x -> 
      let (v, cl, context_stack) = lookup graph x cl context_stack in
      Environment.add env x "0";
      Clause(x, Value_body(substitute v cl graph context_stack (Environment.create 10))))
  |> List.of_enum
;;

let eval (Expr(cls)) =
  let context_stack = Unbounded_Stack.empty in
  let initial_graph = Wddpac_graph.empty in 
  let index = initialize_graph cls initial_graph None 1 in 
  let rx = rv cls in
  let (v, cl, context_stack) = lookup initial_graph rx (None, index) context_stack in
  substitute v cl initial_graph context_stack (Environment.create 10)
;;
