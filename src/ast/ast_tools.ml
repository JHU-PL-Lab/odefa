open Batteries;;

open Ast;;

(** Returns a list of all clauses that occur in expression, deeply traversing
    the syntax tree. *)
let rec flatten (Expr clauses) =
  match clauses with
  | [] ->
    []
  | ((Clause (_, Value_body (Value_function (Function_value (_, function_body))))) as clause) :: rest_clauses ->
    clause :: flatten function_body @ flatten (Expr rest_clauses)
  | ((Clause (_, Conditional_body(_, _,
                                  Function_value (_, match_body),
                                  Function_value (_, antimatch_body)))) as clause) :: rest_clauses ->
    clause :: flatten match_body @ flatten antimatch_body @ flatten (Expr rest_clauses)
  | clause :: rest_clauses ->
    clause :: flatten (Expr rest_clauses)
;;

(** Returns a list of clauses that occur in the immediate block, shallowly
    traversing the syntax tree and inlining conditionals only. *)
let rec flatten_immediate_block (Expr clauses) =
  match clauses with
  | [] ->
    []
  | ((Clause (_, Conditional_body(_, _,
                                  Function_value (_, match_body),
                                  Function_value (_, antimatch_body)))) as clause) :: rest_clauses ->
    clause :: flatten_immediate_block match_body @ flatten_immediate_block antimatch_body @ flatten_immediate_block (Expr rest_clauses)
  | clause :: rest_clauses ->
    clause :: flatten_immediate_block (Expr rest_clauses)
;;

(** Returns the set of immediate variable bindings that occur in expression,
    shallowly traversing the syntax tree. *)
let defined_variables (Expr clauses) =
  clauses
  |> List.map (fun (Clause (bound_variable, _)) -> bound_variable)
  |> Var_set.of_list
;;

(** Returns a list of all variable bindings that occur in expression, including
    repeated ones, deeply traversing the syntax tree. *)
let bindings_with_repetition expression =
  flatten expression
  |> List.map
    (
      function
      | Clause (bound_variable, Value_body (Value_function (Function_value (formal_parameter, _)))) ->
        [bound_variable; formal_parameter]
      | Clause (bound_variable, Conditional_body(_, _,
                                                 Function_value (match_formal_parameter, _),
                                                 Function_value (antimatch_formal_parameter, _))) ->
        [bound_variable; match_formal_parameter; antimatch_formal_parameter]
      | Clause (bound_variable, _) ->
        [bound_variable]
    )
  |> List.flatten
;;

(** Returns the set of variable bindings that occur in expression, deeply
    traversing the syntax tree. *)
let bindings expression =
  Var_set.of_list @@ bindings_with_repetition expression
;;

(** Returns the set of variables that have use occurrences in expression, deeply
    traversing the syntax tree. *)
let use_occurrences expression =
  flatten expression
  |> List.map (
    fun (Clause (_, clause_body)) ->
      match clause_body with
      | Value_body _ ->
        Var_set.empty
      | Var_body variable ->
        Var_set.singleton variable
      | Appl_body (function_, actual_parameter) ->
        Var_set.of_list [function_; actual_parameter]
      | Conditional_body (subject, _, _, _) ->
        Var_set.singleton subject
      | Projection_body (var, _) ->
        Var_set.singleton var
      | Deref_body var ->
        Var_set.singleton var
      | Update_body (cell_var, value_var) ->
        Var_set.of_list [cell_var; value_var]
      | Unary_operation_body (_, operand) ->
        Var_set.singleton operand
      | Binary_operation_body (left_operand, _, right_operand) ->
        Var_set.of_list [left_operand; right_operand]
  )
  |> List.fold_left Var_set.union Var_set.empty
;;

(** Returns the set of bindings repeated in expression, deeply traversing the
    syntax tree. *)
let non_unique_bindings expression =
  bindings_with_repetition expression
  |> List.group compare_var
  |> List.filter_map (
    fun group ->
      if List.length group > 1 then
        Some (List.first group)
      else
        None
  )
  |> Var_set.of_list
;;

let _bind_filt bound site_x vars =
  vars
  |> List.filter (fun x -> not @@ Ident_set.mem x bound)
  |> List.map (fun x -> (site_x, x))
;;

let rec check_scope_expr
    (bound : Ident_set.t) (e : expr)
  : (ident * ident) list =
  let Expr(cls) = e in
  snd @@
  List.fold_left
    (fun (bound',result) clause ->
       let result' = result @ check_scope_clause bound' clause in
       let Clause(Var(x,_),_) = clause in
       let bound'' = Ident_set.add x bound' in
       (bound'', result')
    )
    (bound, [])
    cls

and check_scope_clause
    (bound : Ident_set.t) (c : clause)
  : (ident * ident) list =
  let Clause(Var(site_x,_),b) = c in
  check_scope_clause_body bound site_x b

and check_scope_clause_body
    (bound : Ident_set.t) (site_x : ident) (b : clause_body)
  : (ident * ident) list =
  match b with
  | Value_body v ->
    begin
      match v with
      | Value_function(Function_value(Var(x',_),e)) ->
        check_scope_expr (Ident_set.add x' bound) e
      | Value_record(Record_value(m)) ->
        m
        |> Ident_map.values
        |> Enum.filter_map
          (fun (Var(x,_)) ->
             if Ident_set.mem x bound then None else Some(site_x, x)
          )
        |> List.of_enum
      | _ ->
        []
    end
  | Var_body (Var(x,_)) -> _bind_filt bound site_x [x]
  | Appl_body (Var(x1,_),Var(x2,_)) -> _bind_filt bound site_x [x1;x2]
  | Conditional_body (Var(x,_), _, f1, f2) ->
    _bind_filt bound site_x [x] @
    check_scope_function_value bound f1 @
    check_scope_function_value bound f2
  | Projection_body (Var(x,_), _) ->
    _bind_filt bound site_x [x]
  | Deref_body(Var(x,_)) ->
    _bind_filt bound site_x [x]
  | Update_body(Var(x1,_), Var(x2,_)) ->
    _bind_filt bound site_x [x1; x2]
  | Unary_operation_body (_, Var(x,_)) ->
    _bind_filt bound site_x [x]
  | Binary_operation_body (Var(x1,_), _, Var(x2,_)) ->
    _bind_filt bound site_x [x1;x2]

and check_scope_function_value
    (bound : Ident_set.t) (f : function_value)
  : (ident * ident) list =
  let Function_value(Var(x,_),e) = f in
  check_scope_expr (Ident_set.add x bound) e
;;

(** Returns a list of pairs of variables. The pair represents a violation on the
    concept of scope, i.e., a variable used that was not in scope. The first
    variable is the program point in which the violation occurred, the second
    variable is the one that was not in scope. *)
let scope_violations expression =
  check_scope_expr Ident_set.empty expression
  |> List.map (fun (i1,i2) -> (Var(i1,None)),Var(i2,None))
;;

(** Returns the last defined variable in a list of clauses. *)
let rv (cs : clause list) : Var.t =
  let Clause(x,_) = List.last cs in x
;;

(** Returns the last defined variable in an expression. *)
let retv (e : expr) : Var.t =
  let Expr(cs) = e in rv cs
;;

(** Homomorphically maps all variables in an expression. *)
let rec map_expr_vars (fn : Var.t -> Var.t) (e : expr) : expr =
  let Expr(cls) = e in Expr(List.map (map_clause_vars fn) cls)

and map_clause_vars (fn : Var.t -> Var.t) (c : clause) : clause =
  let Clause(x,b) = c in Clause(fn x, map_clause_body_vars fn b)

and map_clause_body_vars (fn : Var.t -> Var.t) (b : clause_body) : clause_body =
  match (b : clause_body) with
  | Value_body v -> Value_body (map_value_vars fn v)
  | Var_body x -> Var_body (fn x)
  | Appl_body (x1,x2) -> Appl_body(fn x1, fn x2)
  | Conditional_body (x, p, f1, f2) ->
    Conditional_body (fn x, p, map_function_vars fn f1, map_function_vars fn f2)
  | Projection_body (x, l) -> Projection_body(fn x, l)
  | Deref_body x -> Deref_body (fn x)
  | Update_body (x1, x2) -> Update_body (fn x1, fn x2)
  | Binary_operation_body (x1, op, x2) ->
    Binary_operation_body (fn x1, op, fn x2)
  | Unary_operation_body (op, x) ->
    Unary_operation_body (op, fn x)

and map_value_vars (fn : Var.t -> Var.t) (v : value) : value =
  match (v : value) with
  | Value_record(Record_value(m)) ->
    Value_record(Record_value(Ident_map.map fn m))
  | Value_function f -> Value_function(map_function_vars fn f)
  | Value_ref(Ref_value x) -> Value_ref (Ref_value(fn x))
  | Value_int _ -> v
  | Value_bool _ -> v
  | Value_string _ -> v

and map_function_vars (fn : Var.t -> Var.t) (f : function_value)
  : function_value =
  let Function_value(x,e) = f in
  Function_value(fn x, map_expr_vars fn e)
;;

(** Mostly-homomorphically operates on every subexpression of an expression.
    Expressions are modified in a bottom-up fashion. *)
let rec transform_exprs_in_expr (fn : expr -> expr) (e : expr) : expr =
  let Expr(cls) = e in
  fn @@ Expr(List.map (transform_exprs_in_clause fn) cls)

and transform_exprs_in_clause (fn : expr -> expr) (c : clause) : clause =
  let Clause(x,b) = c in Clause(x, transform_exprs_in_clause_body fn b)

and transform_exprs_in_clause_body (fn : expr -> expr) (b : clause_body)
  : clause_body =
  match (b : clause_body) with
  | Value_body v -> Value_body (transform_exprs_in_value fn v)
  | Var_body _ -> b
  | Appl_body (_, _) -> b
  | Conditional_body (x, p, f1, f2) ->
    Conditional_body (x, p,
                      transform_exprs_in_function fn f1,
                      transform_exprs_in_function fn f2)
  | Projection_body (_, _) -> b
  | Deref_body _ -> b
  | Update_body (_, _) -> b
  | Binary_operation_body (_, _, _) -> b
  | Unary_operation_body (_, _) -> b

and transform_exprs_in_value (fn : expr -> expr) (v : value) : value =
  match (v : value) with
  | Value_record _ -> v
  | Value_function f -> Value_function(transform_exprs_in_function fn f)
  | Value_ref _ -> v
  | Value_int _ -> v
  | Value_bool _ -> v
  | Value_string _ -> v

and transform_exprs_in_function (fn : expr -> expr) (fv : function_value)
  : function_value =
  let Function_value(x,e) = fv in
  Function_value(x, transform_exprs_in_expr fn e)
;;
