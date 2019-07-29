(** Defines the interpreter for this analysis. *)

open Odefa_ast;;

open Adi_types;;
open Ast;;

module Make
    (S : Specification)
    (T : Adi_structure_types.Sig with module S = S)
    (M : Adi_monad.Sig with module S = S and module T = T) =
struct
  module S = S;;
  open M;;

  type timestamp = Var.t list;;

  let lift_value (v : value) : T.abstract_value m =
    match v with
    | Value_record _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
    | Value_function _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
    | Value_ref _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
    | Value_int _ ->
      return T.Abstract_int
    | Value_bool _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
    | Value_string _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "lift_value"
  ;;

  let evaluate_clause (c : clause) : T.abstract_value m =
    let Clause(Var(x, _), b) = c in
    match b with
    | Value_body v ->
      let%bind address = allocate x in
      bind_variable x address @@
      let%bind av = lift_value v in
      let%bind () = store_set address av in
      return av
    | Var_body (Var(x', _)) ->
      let%bind address = read_variable x' in
      let%bind av = store_get address in
      let%bind address' = allocate x in
      bind_variable x address' @@
      let%bind () = store_set address' av in
      return av
    | Appl_body (Var(x',_), Var(x'', _)) ->
      ignore x'; ignore x'';
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
    | Conditional_body (_, _, _, _) ->
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
    | Projection_body (_, _) ->
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
    | Deref_body _ ->
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
    | Update_body (_, _) ->
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
    | Binary_operation_body (_, _, _) ->
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
    | Unary_operation_body (_, _) ->
      raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
  ;;

  let evaluate (e : expr) : T.abstract_value m =
    let Expr(cls) = e in
    let rec loop cls =
      match cls with
      | [] ->
        (* The expression is exhausted.  Use the value most recently bound in
           the store to produce the expression's result. *)
        raise @@ Jhupllib.Utils.Invariant_failure "empty expression"
      | [cl] ->
        (* This is the last clause; return its bound value for the
           expression. *)
        evaluate_clause cl
      | cl :: cls' ->
        (* This is not the last clause; ignore the value it binds. *)
        let%bind _ = evaluate_clause cl in
        loop cls'
    in
    loop cls
  ;;
end;;
