(**
   A module defining data structures and basic operations to form a Plume graph.
*)

open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Odefa_ast;;

open Abstract_ast;;
open Ast;;
open Pp_utils;;

type plume_edge =
  | Plume_edge of annotated_clause * annotated_clause
  [@@deriving ord, show, to_yojson]
;;

module Plume_edge =
struct
  type t = plume_edge
  let compare = compare_plume_edge
  let pp = pp_plume_edge
  let to_yojson = plume_edge_to_yojson
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  type plume_graph

  val empty : plume_graph

  val add_edge : plume_edge -> plume_graph -> plume_graph

  val edges_of : plume_graph -> plume_edge Enum.t

  val has_edge : plume_edge -> plume_graph -> bool

  val edges_from : annotated_clause -> plume_graph -> plume_edge Enum.t

  val edges_to : annotated_clause -> plume_graph -> plume_edge Enum.t

  val preds : annotated_clause -> plume_graph -> annotated_clause Enum.t

  val succs : annotated_clause -> plume_graph -> annotated_clause Enum.t

  val to_yojson : plume_graph -> Yojson.Safe.json
end;;

(* TODO: improve the performance of this implementation! *)
module Graph_impl : Graph_sig =
struct
  module Plume_edge_set =
  struct
    module Impl = Set.Make(Plume_edge);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(Plume_edge);;
    include Yojson_utils.Set_to_yojson(Impl)(Plume_edge);;
  end;;

  type plume_graph = Graph of Plume_edge_set.t [@@deriving to_yojson];;

  let empty = Graph(Plume_edge_set.empty);;

  let add_edge edge (Graph(s)) = Graph(Plume_edge_set.add edge s);;

  let edges_of (Graph(s)) = Plume_edge_set.enum s;;

  let has_edge edge (Graph(s)) = Plume_edge_set.mem edge s;;

  let edges_from acl (Graph(s)) =
    Plume_edge_set.enum s
    |> Enum.filter (fun (Plume_edge(acl',_)) -> equal_annotated_clause acl acl')
  ;;

  let succs acl g =
    edges_from acl g |> Enum.map (fun (Plume_edge(_,acl)) -> acl)
  ;;

  let edges_to acl (Graph(s)) =
    Plume_edge_set.enum s
    |> Enum.filter (fun (Plume_edge(_,acl')) -> equal_annotated_clause acl acl')
  ;;

  let preds acl g =
    edges_to acl g |> Enum.map (fun (Plume_edge(acl,_)) -> acl)
  ;;

  let to_yojson = plume_graph_to_yojson;;
end;;

include Graph_impl;;

let pp_plume_graph formatter g =
  pp_concat_sep_delim "{" "}" ", " pp_plume_edge formatter @@ edges_of g
;;

let rec lift_expr (Expr(cls)) =
  Abs_expr(List.map lift_clause cls)

and lift_clause (Clause(x,b)) =
  Abs_clause(lift_var x, lift_clause_body b)

and lift_clause_body b =
  match b with
  | Value_body v -> Abs_value_body(lift_value v)
  | Var_body x -> Abs_var_body(lift_var x)
  | Appl_body(x,x') -> Abs_appl_body(lift_var x, lift_var x')
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