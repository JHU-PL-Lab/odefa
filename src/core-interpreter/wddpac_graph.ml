(**
   A module defining data structures and basic operations to form a DDPA graph.
*)

(*
  Adjacency list representation
  Annotated_Clause -> the nodes/clauses that are directly behind it.
  if a << b then b maps to a
*)
open Batteries;;
(* open Hashtbl;; *)

(* open Jhupllib;; *)

open Core_ast;;
(* open Pp_utils;; *)

type annotated_clause =
  | Unannotated_clause of clause
  | Enter_clause of var * var * clause
  | Exit_clause of var * var * clause
  | Start_clause of var
  (** This variable is the return variable of the block that this clause
      starts. *)
  | End_clause of var
  (** This variable is the return variable of the block that this clause
      ends. *)
[@@deriving ord, eq, to_yojson]
;;

module Annotated_Clause =
struct
  type t = var
  let equal = equal_var
  let hash = Hashtbl.hash
end;;

type graph_node =
  | Graph_node of annotated_clause
;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  type wddpac_graph

  val empty : wddpac_graph

  val add_edge : annotated_clause -> annotated_clause -> wddpac_graph -> unit

  val lookup : var -> wddpac_graph -> annotated_clause

end;;

module Graph_impl : Graph_sig =
struct

  module Edge_tbl = Hashtbl.Make(Annotated_clause)

  type wddpac_graph = Graph of annotated_clause Edge_tbl.t;;

  let empty = Graph(Edge_tbl.create 10);;

  let add_edge cl1 cl2 (Graph(g)) =
    Edge_tbl.add g cl1 cl2
  ;;

  let lookup var (Graph(g)) =
    Wddpac_edge_tbl.find g var
  ;;
end;;

include Graph_impl;;
