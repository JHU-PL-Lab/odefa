(**
   A module defining data structures and basic operations to form a DDPA graph.
*)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ddpa_abstract_ast;;
open Pp_utils;;

type ddpa_edge =
  | Ddpa_edge of annotated_clause * annotated_clause
[@@deriving ord, show, to_yojson]
;;

module Ddpa_edge =
struct
  type t = ddpa_edge
  let compare = compare_ddpa_edge
  let pp = pp_ddpa_edge
  let to_yojson = ddpa_edge_to_yojson
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  type ddpa_graph

  val empty : ddpa_graph

  val add_edge : ddpa_edge -> ddpa_graph -> ddpa_graph

  val edges_of : ddpa_graph -> ddpa_edge Enum.t

  val has_edge : ddpa_edge -> ddpa_graph -> bool

  val edges_from : annotated_clause -> ddpa_graph -> ddpa_edge Enum.t

  val edges_to : annotated_clause -> ddpa_graph -> ddpa_edge Enum.t

  val preds : annotated_clause -> ddpa_graph -> annotated_clause Enum.t

  val succs : annotated_clause -> ddpa_graph -> annotated_clause Enum.t

  val to_yojson : ddpa_graph -> Yojson.Safe.t
end;;

(* TODO: improve the performance of this implementation! *)
module Graph_impl : Graph_sig =
struct
  module Ddpa_edge_set =
  struct
    module Impl = Set.Make(Ddpa_edge);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(Ddpa_edge);;
    include Yojson_utils.Set_to_yojson(Impl)(Ddpa_edge);;
  end;;

  module Annotated_clause_to_edge_multimap = struct
    module Impl = Multimap.Make(Annotated_clause)(Ddpa_edge);;
    include Impl;;
    include Multimap_pp.Make(Impl)(Annotated_clause)(Ddpa_edge);;
    include Multimap_to_yojson.Make(Impl)(Annotated_clause)(Ddpa_edge);;
  end;;

  type ddpa_graph =
    { dg_all_edges : Ddpa_edge_set.t;
      dg_edges_from : Annotated_clause_to_edge_multimap.t;
      dg_edges_to : Annotated_clause_to_edge_multimap.t;
    }
  [@@deriving to_yojson];;

  let empty =
    { dg_all_edges = Ddpa_edge_set.empty;
      dg_edges_from = Annotated_clause_to_edge_multimap.empty;
      dg_edges_to = Annotated_clause_to_edge_multimap.empty;
    }
  ;;

  let add_edge edge g =
    let Ddpa_edge(from_node,to_node) = edge in
    { dg_all_edges = Ddpa_edge_set.add edge g.dg_all_edges;
      dg_edges_from =
        Annotated_clause_to_edge_multimap.add from_node edge g.dg_edges_from;
      dg_edges_to =
        Annotated_clause_to_edge_multimap.add to_node edge g.dg_edges_to;
    }
  ;;

  let edges_of g = g.dg_all_edges |> Ddpa_edge_set.enum;;

  let has_edge edge g = Ddpa_edge_set.mem edge g.dg_all_edges;;

  let edges_from acl g =
    Annotated_clause_to_edge_multimap.find acl g.dg_edges_from
  ;;

  let succs acl g =
    edges_from acl g |> Enum.map (fun (Ddpa_edge(_,acl)) -> acl)
  ;;

  let edges_to acl g =
    Annotated_clause_to_edge_multimap.find acl g.dg_edges_to
  ;;

  let preds acl g =
    edges_to acl g |> Enum.map (fun (Ddpa_edge(acl,_)) -> acl)
  ;;

  let to_yojson = ddpa_graph_to_yojson;;
end;;

include Graph_impl;;

let pp_ddpa_graph formatter g =
  pp_concat_sep_delim "{" "}" ", " pp_ddpa_edge formatter @@ edges_of g
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
