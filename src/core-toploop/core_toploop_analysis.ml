open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_toploop_analysis_types;;
open Core_toploop_ddpa_wrapper_types;;
open Core_toploop_utils;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;
open Ddpa_graph;;
open Ddpa_utils;;

module Make(DDPA_wrapper : DDPA_wrapper) =
struct
  module DDPA_wrapper = DDPA_wrapper;;

  let find_errors analysis =
    let open Eager_nondeterminism.Nondeterminism_monad in
    enum @@
    let%bind acl =
      analysis
      |> DDPA_wrapper.expression_of
      |> lift_expr
      |> iterate_abstract_clauses
      |> pick_enum
    in
    let Abs_clause(x_clause,b) = acl in
    let lookup x =
      DDPA_wrapper.values_of_variable_from x (Unannotated_clause(acl)) analysis
    in
    match b with
    | Abs_value_body _
    | Abs_var_body _
    | Abs_conditional_body _ ->
      (* There's nothing this body that can be inconsistent. *)
      zero ()
    | Abs_appl_body(xf,xa) ->
      let%bind store = pick_enum @@ Abstract_store_set.enum @@ lookup xf in
      begin
        match store_read store  with
        | Abs_value_function _ -> zero ()
        | _ ->
          return @@ Application_of_non_function(x_clause,xf,store,lookup xa)
      end
    | Abs_projection_body(x,i) ->
      let%bind store = pick_enum @@ Abstract_store_set.enum @@ lookup x in
      begin
        match store_read store with
        | Abs_value_record(Abs_record_value(m)) ->
          if Ident_map.mem i m
          then zero ()
          else return @@ Projection_of_absent_label(x_clause,x,store,i)
        | _ -> return @@ Projection_of_non_record(x_clause,x,store)
      end
    | Abs_deref_body(x)->
      let%bind store = pick_enum @@ Abstract_store_set.enum @@ lookup x in
      begin
        match store_read store with
        | Abs_value_ref _ -> zero ()
        | _ -> return @@ Deref_of_non_ref(x_clause,x,store)
      end
    | Abs_update_body(x,_) ->
      let%bind store = pick_enum @@ Abstract_store_set.enum @@ lookup x in
      begin
        match store_read store with
        | Abs_value_ref _ -> zero ()
        | _ -> return @@ Update_of_non_ref(x_clause,x,store)
      end
    | Abs_binary_operation_body(x1,op,x2) ->
      let%bind store1 = pick_enum @@ Abstract_store_set.enum @@ lookup x1 in
      let%bind store2 = pick_enum @@ Abstract_store_set.enum @@ lookup x2 in
      let v1 = store_read store1 in
      let v2 = store_read store2 in
      let is_valid = Option.is_some (abstract_binary_operation op v1 v2) in
      if is_valid
      then zero ()
      else return @@
        Invalid_binary_operation(x_clause, op, x1, store1, x2, store2)
    | Abs_unary_operation_body(op,x) ->
      let%bind store = pick_enum @@ Abstract_store_set.enum @@ lookup x in
      begin
        match (op, store_read store) with
        | (Unary_operator_bool_not, Abs_value_bool _) -> zero ()
        | (Unary_operator_bool_coin_flip, _) -> zero ()
        | _ -> return @@ Invalid_unary_operation(x_clause, op, x, store)
      end
end;;
