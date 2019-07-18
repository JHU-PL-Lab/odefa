open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_abstract_ast;;

open Abstract_ast;;
open Abstract_ast_utils;;
open Ast;;
open Plume_graph;;

let is_immediate acl =
  match acl with
  | Unannotated_clause(abs_clause) -> is_abstract_clause_immediate abs_clause
  | Enter_clause _
  | Exit_clause _
  | Start_clause _
  | End_clause _ -> true
;;

module type Graph_wiring =
sig

  module G : Graph_sig;;

  val wire_fun : G.node -> abstract_function_value -> abstract_var ->
    abstract_var -> G.t -> (G.edge Enum.t * G.node * G.node * G.node);;
  val wire_cond : G.node -> abstract_function_value -> abstract_var ->
    abstract_var -> G.t -> (G.edge Enum.t);;

end;;

module Graph_construct (G : Graph_sig) : Graph_wiring with module G = G =
struct

  module G = G;;

  open G;;

  module C = G.E.C;;

  module E = G.E;;

  open E;;

  let wire_fun site_node func x1 x2 graph : (edge Enum.t * node * node * node) =
    let Node(acl, ctx) = site_node in
    begin
      match acl with
      | Unannotated_clause abcl ->
        let new_context = C.push abcl ctx in
        let Abs_function_value(x0, Abs_expr(body)) = func in
        let wire_in_node = Node(Enter_clause(x0, x1, abcl), new_context) in
        let start_node = Node(Start_clause (rv body), new_context) in
        let end_node = Node(End_clause (rv body), new_context) in
        let wire_out_node = Node(Exit_clause(x2,rv body, abcl), new_context) in
        let pred_edges =
          preds site_node graph
          |> Enum.map (fun node' -> Edge(node', wire_in_node))
        in
        let succ_edges =
          succs site_node graph
          |> Enum.map (fun node' -> Edge(wire_out_node, node'))
        in
        let inner_edges =
          List.enum body
          |> Enum.map (fun cl -> Node(Unannotated_clause(cl), new_context))
          |> Enum.append (Enum.singleton start_node)
          |> Enum.append (Enum.singleton wire_in_node)
          |> flip Enum.append (Enum.singleton end_node)
          |> flip Enum.append (Enum.singleton wire_out_node)
          |> Utils.pairwise_enum_fold
            (fun node1 node2 -> Edge(node1, node2))
        in
        let new_edges = Enum.append pred_edges @@ Enum.append inner_edges succ_edges in
        (new_edges, site_node, wire_in_node, wire_out_node)
      | _ -> raise @@
        Jhupllib.Utils.Invariant_failure "Error: Call site should be Unannotated_clause"
    end
  ;;

  let wire_cond site_node func x1 x2 graph =
    let Node(acl, ctx) = site_node in
    begin
      match acl with
      | Unannotated_clause abcl ->
        let Abs_function_value(x0, Abs_expr(body)) = func in
        let wire_in_node = Node(Enter_clause(x0, x1, abcl), ctx) in
        let start_node = Node(Start_clause (rv body), ctx) in
        let end_node = Node(End_clause (rv body), ctx) in
        let wire_out_node = Node(Exit_clause(x2,rv body, abcl), ctx) in
        let pred_edges =
          preds site_node graph
          |> Enum.map (fun node' -> Edge(node', wire_in_node))
        in
        let succ_edges =
          succs site_node graph
          |> Enum.map (fun node' -> Edge(wire_out_node, node'))
        in
        let inner_edges =
          List.enum body
          |> Enum.map (fun cl -> Node(Unannotated_clause(cl), ctx))
          |> Enum.append (Enum.singleton start_node)
          |> Enum.append (Enum.singleton wire_in_node)
          |> flip Enum.append (Enum.singleton end_node)
          |> flip Enum.append (Enum.singleton wire_out_node)
          |> Utils.pairwise_enum_fold
            (fun node1 node2 -> Edge(node1, node2))
        in
        Enum.append pred_edges @@ Enum.append inner_edges succ_edges
      | _ -> raise @@
        Jhupllib.Utils.Invariant_failure "Error: Call site should be Unannotated_clause"
    end
  ;;

end;;

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
