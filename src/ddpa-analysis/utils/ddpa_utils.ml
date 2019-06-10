open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_abstract_ast;;

open Ast;;
open Abstract_ast;;
open Abstract_ast_utils;;
open Ddpa_graph;;

let is_immediate acl =
  match acl with
  | Unannotated_clause(abs_clause) -> is_abstract_clause_immediate abs_clause
  | Enter_clause _
  | Exit_clause _
  | Start_clause _
  | End_clause _ -> true
;;

let wire site_cl func x1 x2 graph =
  let site_acl = Unannotated_clause(site_cl) in
  let Abs_function_value(x0, Abs_expr(body)) = func in
  let wire_in_acl = Enter_clause(x0,x1,site_cl) in
  let start_acl = Start_clause (rv body) in
  let end_acl = End_clause (rv body) in
  let wire_out_acl = Exit_clause(x2,rv body,site_cl) in
  let pred_edges =
    Ddpa_graph.preds site_acl graph
    |> Enum.map (fun acl' -> Ddpa_edge(acl',wire_in_acl))
  in
  let succ_edges =
    Ddpa_graph.succs site_acl graph
    |> Enum.map (fun acl' -> Ddpa_edge(wire_out_acl,acl'))
  in
  let inner_edges =
    List.enum body
    |> Enum.map (fun cl -> Unannotated_clause(cl))
    |> Enum.append (Enum.singleton start_acl)
    |> Enum.append (Enum.singleton wire_in_acl)
    |> flip Enum.append (Enum.singleton end_acl)
    |> flip Enum.append (Enum.singleton wire_out_acl)
    |> Utils.pairwise_enum_fold
      (fun acl1 acl2 -> Ddpa_edge(acl1,acl2))
  in
  Enum.append pred_edges @@ Enum.append inner_edges succ_edges
;;

(**
   Defines immediate pattern matching.
*)
let immediately_matched_by (v : abstract_value) : Pattern_set.t option =
  match v with
  | Abs_value_function _ ->
    Some (Pattern_set.of_list [ Any_pattern ; Fun_pattern ])
  | Abs_value_int ->
    Some (Pattern_set.of_list [ Any_pattern ; Int_pattern ])
  | Abs_value_bool b ->
    Some (Pattern_set.of_list [ Any_pattern ; Bool_pattern b ])
  | Abs_value_string ->
    Some (Pattern_set.of_list [ Any_pattern ; String_pattern ])
  | Abs_value_ref _ ->
    Some (Pattern_set.of_list [ Any_pattern ; Ref_pattern ])
  | Abs_value_record _ ->
    None
;;

(**
   Defines the behavior of binary operations in abstract evaluation.
*)
let abstract_binary_operation
    (binop : binary_operator)
    (arg1 : abstract_value)
    (arg2 : abstract_value)
  : abstract_value Enum.t option =
  let singleton x = Some(Enum.singleton x) in
  match binop,arg1,arg2 with
  | ( Binary_operator_plus
    | Binary_operator_int_minus
    ), Abs_value_int, Abs_value_int ->
    singleton Abs_value_int
  | ( Binary_operator_int_less_than
    | Binary_operator_int_less_than_or_equal_to
    | Binary_operator_equal_to
    ), Abs_value_int, Abs_value_int ->
    Some (List.enum [Abs_value_bool(true); Abs_value_bool(false)])
  | Binary_operator_equal_to, Abs_value_bool b1, Abs_value_bool b2 ->
    singleton @@ Abs_value_bool(b1 = b2)
  | Binary_operator_bool_and, Abs_value_bool b1, Abs_value_bool b2 ->
    singleton @@ Abs_value_bool(b1 && b2)
  | Binary_operator_bool_or, Abs_value_bool b1, Abs_value_bool b2 ->
    singleton @@ Abs_value_bool(b1 || b2)
  | Binary_operator_equal_to, Abs_value_string, Abs_value_string ->
    Some (List.enum [Abs_value_bool(true); Abs_value_bool(false)])
  | Binary_operator_plus, Abs_value_string, Abs_value_string ->
    singleton @@ Abs_value_string
  | Binary_operator_index, Abs_value_string, Abs_value_int ->
    singleton @@ Abs_value_string
  | _ -> None
;;

(**
   Defines the behavior of unary operations in abstract evaluation.
*)
let abstract_unary_operation
    (unop : unary_operator)
    (arg : abstract_value)
  : abstract_value Enum.t option =
  let singleton x = Some(Enum.singleton x) in
  match unop,arg with
  | Unary_operator_bool_not, Abs_value_bool b ->
    singleton @@ Abs_value_bool(not b)
  | _ -> None
;;
