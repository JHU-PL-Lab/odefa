open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Abstract_ast;;
open Nondeterminism;;

exception Non_record_projection of string;;

let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Abs_clause(x,_) = List.last body in x
;;

let negative_pattern_set_selection record_type pattern_set =
  let (Abs_record_value m) = record_type in
  let record_labels = Ident_set.of_enum @@ Ident_map.keys m in
  let relevant_patterns = pattern_set
                          |> Pattern_set.enum
                          |> Enum.filter
                            (fun pattern ->
                               match pattern with
                               | Record_pattern m' ->
                                 Ident_set.subset (Ident_set.of_enum @@
                                                   Ident_map.keys m')
                                   record_labels
                               | Any_pattern ->
                                 raise @@ Utils.Invariant_failure
                                   "Shouldn't call `negative_pattern_set_selection' with a pattern set that contains `any' patterns."
                               | _ -> false)
  in
  (* This function selects a single label from a given pattern and constructs
     a pattern from it. *)
  let pick_pattern pattern =
    match pattern with
    | Record_pattern m' ->
      let open Nondeterminism_monad in
      let%bind (k,v) = pick_enum @@ Ident_map.enum m' in
      return @@ Record_pattern(Ident_map.singleton k v)
    | _ -> raise @@ Utils.Invariant_failure (
        Printf.sprintf "The non-record pattern `%s' ended up on `analysis.ml:negative_pattern_set_selection:pick_pattern'."
          (show_pattern pattern))
  in
  let open Nondeterminism_monad in
  let%bind selected_patterns =
    Nondeterminism_monad.mapM pick_pattern relevant_patterns
  in
  return @@ Pattern_set.of_enum selected_patterns
;;


(* `pattern' /must/ satisfy the `is_record_pattern_set' predicate. Note that
   the results of `negative_pattern_set_selection' already satisfy it. *)
let pattern_projection pattern label =
  match pattern with
  | Record_pattern m ->
    begin
      try
        Some (Ident_map.find label m)
      with
      | Not_found -> None
    end
  | Any_pattern -> None
  | _ -> raise @@ Non_record_projection (
      Printf.sprintf "Tried to project out of a non-record pattern `%s' in `analysis.ml:pattern_projection'."
        (show_pattern pattern))
;;

let pattern_set_projection set label =
  set
  |> Pattern_set.enum
  |> Enum.map (flip pattern_projection label)
  |> Enum.filter_map identity
  |> Pattern_set.of_enum
;;

let is_record_pattern_set set =
  set
  |> Pattern_set.enum
  |> Enum.for_all
    (
      fun pattern ->
        match pattern with
        | Record_pattern _ | Any_pattern -> true
        | _ -> false
    )
;;

let labels_in_record (Abs_record_value m) =
  Ident_set.of_enum @@ Ident_map.keys m
;;

(* `pattern' /must/ satisfy the `is_record_pattern_set' predicate. Note that
   the results of `negative_pattern_set_selection' already satisfy it. *)
let labels_in_pattern pattern =
  match pattern with
  | Record_pattern m ->
    Ident_set.of_enum @@ Ident_map.keys m
  | Any_pattern ->
    Ident_set.empty
  | _ -> raise @@ Non_record_projection (
      Printf.sprintf "Tried to enumerate labels out of a non-record pattern `%s' in `analysis.ml:labels_in_pattern'."
        (show_pattern pattern))
;;

let labels_in_pattern_set set =
  set
  |> Pattern_set.enum
  |> Enum.map labels_in_pattern
  |> Enum.fold Ident_set.union Ident_set.empty
;;

let rec lift_expr (Expr(cls)) =
  Abs_expr(List.map lift_clause cls)

and lift_clause (Clause(x,b)) =
  Abs_clause(lift_var x, lift_clause_body b)

and lift_clause_body b =
  match b with
  | Value_body v -> Abs_value_body(lift_value v)
  | Var_body x -> Abs_var_body(lift_var x)
  | Appl_body(x,x',annots) -> Abs_appl_body(lift_var x, lift_var x', annots)
  | Conditional_body(x,p,f1,f2) ->
    Abs_conditional_body(lift_var x,p,lift_function_value f1,lift_function_value f2)
  | Projection_body(x,i) -> Abs_projection_body(lift_var x,i)
  | Deref_body(x) -> Abs_deref_body(lift_var x)
  | Update_body(x,x') -> Abs_update_body(lift_var x, lift_var x')
  | Binary_operation_body(x1,op,x2) ->
    Abs_binary_operation_body(lift_var x1, op, lift_var x2)
  | Unary_operation_body(op,x1) -> Abs_unary_operation_body(op, lift_var x1)

and lift_value v =
  match v with
  | Value_record r -> Abs_value_record (lift_record_value r)
  | Value_function f -> Abs_value_function(lift_function_value f)
  | Value_ref r -> Abs_value_ref (lift_ref_value r)
  | Value_int _ -> Abs_value_int
  | Value_bool b -> Abs_value_bool b
  | Value_string _ -> Abs_value_string

and lift_var (Var(i,_)) =
  Abs_var i

and lift_function_value (Function_value(x,e)) =
  Abs_function_value(lift_var x, lift_expr e)

and lift_ref_value (Ref_value x) =
  Abs_ref_value(lift_var x)

and lift_record_value (Record_value els) =
  Abs_record_value(Ident_map.map lift_var els)
;;
