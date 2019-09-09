(** Defines the interpreter for this analysis. *)

open Batteries;;
open Odefa_abstract_ast;;
open Odefa_ast;;

open Adi_specification;;
open Ast;;

module Make
    (S : Specification)
    (T : Adi_structure_types.Sig with module C = S.C)
    (M : Adi_monad.Sig with module C = S.C and module T = T) =
struct
  module S = S;;

  module E = S.E(S.C)(T)(M);;

  open M;;

  type timestamp = Var.t list;;

  let lift_value (v : value) : T.abstract_value m =
    match v with
    | Value_record((Record_value map) as rv) ->
      let env_vars =
        lazy begin
          (* Using a junk identifier "!" because snd discards it *)
          Ast_tools.check_scope_record_value Ident_set.empty (Ident "!") rv
          |> List.map snd
          |> Ident_set.of_list (* eliminate duplicates *)
          |> Ident_set.enum
        end
      in
      let%bind env = E.build_environment env_vars in
      return @@ T.Abstract_record(Ident_map.map (fun (Var(x,_)) -> x) map, env)
    | Value_function fv ->
      let env_vars =
        lazy begin
          Ast_tools.check_scope_function_value Ident_set.empty fv
          |> List.map snd
          |> Ident_set.of_list (* eliminate duplicates *)
          |> Ident_set.enum
        end
      in
      let%bind env = E.build_environment env_vars in
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

  let rec matches (v : T.abstract_value) (p : pattern) : bool m =
    match v,p with
    | _, Any_pattern -> return true
    | Abstract_record(vmap, env), Record_pattern(pmap) ->
      pmap
      |> Ident_map.enum
      |> Enum.map
        (fun (lbl, pattern) ->
           match Ident_map.Exceptionless.find lbl vmap with
           | None -> return false
           | Some var ->
             let addr = Ident_map.find var env in
             let%bind value = store_get addr in
             matches value pattern
        )
      |> List.of_enum
      |> sequence
      |> lift (List.fold_left (&&) true)
    | Abstract_record _, _ -> return false
    | Abstract_function _, Fun_pattern -> return true
    | Abstract_function _, _ -> return false
    | Abstract_int, Int_pattern -> return true
    | Abstract_int, _ -> return false
    | Abstract_bool b, Bool_pattern b' -> return (b = b')
    | Abstract_bool _, _ -> return false
    | Abstract_string, String_pattern -> return true
    | Abstract_string, _ -> return false
  ;;

  let perform_binop
      (v1 : T.abstract_value) (op : binary_operator) (v2 : T.abstract_value)
    : T.abstract_value m =
    match v1,op,v2 with
    | Abstract_int,
      (Binary_operator_plus | Binary_operator_int_minus),
      Abstract_int ->
      return T.Abstract_int
    | Abstract_string,
      Binary_operator_plus,
      Abstract_string ->
      return T.Abstract_string
    | Abstract_string,
      Binary_operator_index,
      Abstract_int ->
      return T.Abstract_string
    | Abstract_string,
      Binary_operator_equal_to,
      Abstract_string ->
      pick @@ List.enum [T.Abstract_bool true; T.Abstract_bool false]
    | Abstract_int,
      (Binary_operator_int_less_than |
       Binary_operator_int_less_than_or_equal_to |
       Binary_operator_equal_to),
      Abstract_int ->
      pick @@ List.enum [T.Abstract_bool true; T.Abstract_bool false]
    | Abstract_bool b1,
      Binary_operator_bool_and,
      Abstract_bool b2 ->
      return @@ T.Abstract_bool(b1 && b2)
    | Abstract_bool b1,
      Binary_operator_bool_or,
      Abstract_bool b2 ->
      return @@ T.Abstract_bool(b1 || b2)
    | Abstract_bool b1,
      Binary_operator_equal_to,
      Abstract_bool b2 ->
      return @@ T.Abstract_bool(b1 = b2)
    | _ ->
      (* No value arises from this operator. *)
      zero ()
  ;;

  let perform_unop (op : unary_operator) (v : T.abstract_value)
    : T.abstract_value m =
    match op, v with
    | Unary_operator_bool_not, T.Abstract_bool b ->
      return @@ T.Abstract_bool (not b)
    | _ ->
      (* No value arises from this operator. *)
      zero ()
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
            | Appl_body (Var(x',_), Var(x'', _), annots) ->
              let%bind x'value = lookup x' in
              let%orzero Abstract_function(fv, env') = x'value in
              let Function_value(Var(parameter, _), body) = fv in
              let%bind x''value = lookup x'' in
              (* Determine whether this call site should be handled contextually
                 or acontextually. *)
              let call_site_is_contextual =
                match annots.csa_contextuality with
                | Call_site_contextual -> true
                | Call_site_acontextual -> false
                | Call_site_acontextual_for vars ->
                  not @@ Ident_set.mem parameter vars
              in
              (* Establish the context of the callee at this site.  We do this
                 by selecting the appropriate reader filter for the computation.
              *)
              (if call_site_is_contextual then
                 (* Don't change the context. *)
                 identity
               else
                 (* Extend the context with the call site. *)
                 push_context (Abstract_ast_utils.lift_clause cl)
              ) @@
              with_environment env' @@
              assign parameter x''value @@
              evaluate body
            | Conditional_body (Var(x, _), p, f1, f2) ->
              let%bind x_value = lookup x in
              let%bind x_matches_p = matches x_value p in
              let Function_value(Var(parameter, _), body) =
                if x_matches_p then f1 else f2
              in
              assign parameter x_value @@
              evaluate body
            | Projection_body (Var(x,_), lbl) ->
              let%bind x_value = lookup x in
              let%orzero Abstract_record(rmap,env) = x_value in
              let%orzero Some var = Ident_map.Exceptionless.find lbl rmap in
              let address = Ident_map.find var env in
              store_get address
            | Deref_body _ ->
              raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
            | Update_body (_, _) ->
              raise @@ Jhupllib_utils.Not_yet_implemented "evaluate_clause"
            | Binary_operation_body (Var(x1, _), op, Var(x2, _)) ->
              let%bind x1value = lookup x1 in
              let%bind x2value = lookup x2 in
              perform_binop x1value op x2value
            | Unary_operation_body (op, Var(x, _)) ->
              let%bind x_value = lookup x in
              perform_unop op x_value
          in
          assign x av @@
          continue av
        end
    in
    loop cls
  ;;
end;;
