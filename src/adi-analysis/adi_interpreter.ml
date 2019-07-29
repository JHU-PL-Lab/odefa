(** Defines the interpreter for this analysis. *)

open Batteries;;
open Odefa_abstract_ast;;
open Odefa_ast;;

open Adi_types;;
open Ast;;

module Make
    (C : Context_model)
    (T : Adi_structure_types.Sig with module C = C)
    (M : Adi_monad.Sig with module C = C and module T = T) =
struct
  module C = C;;
  open M;;

  type timestamp = Var.t list;;

  let lift_value (v : value) : T.abstract_value m =
    match v with
    | Value_record _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
    | Value_function fv ->
      let%bind env = capture_environment () in
      return @@ T.Abstract_function(fv, env)
    | Value_ref _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
    | Value_int _ ->
      return T.Abstract_int
    | Value_bool b ->
      return @@ T.Abstract_bool b
    | Value_string _ ->
      return T.Abstract_string
  ;;

  let matches (v : T.abstract_value) (p : pattern) : bool =
    match v,p with
    | _, Any_pattern -> true
    | Abstract_record _, _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "matches"
    | Abstract_function _, Fun_pattern -> true
    | Abstract_function _, _ -> false
    | Abstract_int, Int_pattern -> true
    | Abstract_int, _ -> false
    | Abstract_bool b, Bool_pattern b' -> b = b'
    | Abstract_bool _, _ -> false
    | Abstract_string, String_pattern -> true
    | Abstract_string, _ -> false
  ;;

  let perform_binop
      (v1 : T.abstract_value) (op : binary_operator) (v2 : T.abstract_value)
    : T.abstract_value m =
    match v1,op,v2 with
    | Abstract_int,
      (Binary_operator_plus | Binary_operator_int_minus),
      Abstract_int ->
      return T.Abstract_int
    | Abstract_int,
      (Binary_operator_int_less_than |
       Binary_operator_int_less_than_or_equal_to |
       Binary_operator_equal_to),
      Abstract_int ->
      pick @@ List.enum [T.Abstract_bool true; T.Abstract_bool false]
    | _ ->
      raise @@ Jhupllib.Utils.Not_yet_implemented "perform_binop"
  ;;

  let rec evaluate (e : expr) : T.abstract_value m =
    let Expr(cls) = e in
    let rec loop cls =
      match cls with
      | [] ->
        (* The expression is exhausted.  Use the value most recently bound in
           the store to produce the expression's result. *)
        raise @@ Jhupllib.Utils.Invariant_failure "empty expression"
      | cl :: cls' ->
        let continue av =
          if List.is_empty cls' then return av else loop cls'
        in
        begin
          let Clause(Var(x, _), b) = cl in
          cached_at x @@
          let%bind av =
            match b with
            | Value_body v ->
              lift_value v
            | Var_body (Var(x', _)) ->
              lookup x'
            | Appl_body (Var(x',_), Var(x'', _)) ->
              let%bind x'value = lookup x' in
              let%orzero Abstract_function(fv, env') = x'value in
              let Function_value(Var(parameter, _), body) = fv in
              let%bind x''value = lookup x'' in
              push_context (Abstract_ast_utils.lift_clause cl) @@
              with_environment env' @@
              assign parameter x''value @@
              evaluate body
            | Conditional_body (Var(x, _), p, f1, f2) ->
              let%bind x_value = lookup x in
              let Function_value(Var(parameter, _), body) =
                if matches x_value p then f1 else f2
              in
              assign parameter x_value @@
              evaluate body
            | Projection_body (_, _) ->
              raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
            | Deref_body _ ->
              raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
            | Update_body (_, _) ->
              raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
            | Binary_operation_body (Var(x1, _), op, Var(x2, _)) ->
              let%bind x1value = lookup x1 in
              let%bind x2value = lookup x2 in
              perform_binop x1value op x2value
            | Unary_operation_body (_, _) ->
              raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
          in
          assign x av @@
          continue av
        end
    in
    loop cls
  ;;
end;;
