(**
   A module defining data structures and basic operations to form a DDPA graph.
*)

open Batteries;;

open Test_core_ast;;

type annotated_clause =
  | Unannotated_clause of clause
  | Conditional_clause of var * pattern * var * var * var * var
  | Function of var * var * value
  | Start_clause of var option
and uint_clause =
  | X_add
  | M_add
  | N_add
  [@@deriving ord, eq, to_yojson]
;;

type graph_node =
  | Graph_node of annotated_clause * var option
  [@@deriving ord, eq, to_yojson]
;;

module Annotated_Clause =
struct
  type t = var
  let equal = equal_var
  let hash = Hashtbl.hash
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  type test_wddpac_graph

  val empty : test_wddpac_graph

  val add_edge : var * annotated_clause * var option -> test_wddpac_graph -> unit

  val lookup : var -> test_wddpac_graph -> graph_node

end;;

module Graph_impl : Graph_sig =
struct

  module Wddpac_edge_tbl = Hashtbl.Make(Annotated_Clause)

  type test_wddpac_graph = Graph of graph_node Wddpac_edge_tbl.t;;

  let empty = Graph(Wddpac_edge_tbl.create 10);;

  let add_edge (v, cl, ctx) (Graph(g)) =
    Wddpac_edge_tbl.add g v (Graph_node(cl, ctx))
  ;;

  let lookup var (Graph(g)) =
    Wddpac_edge_tbl.find g var
  ;;
end;;

include Graph_impl;;
