(**
   A module defining data structures and basic operations to form a DDPA graph.
*)

(*
  Adjacency list representation
  Annotated_Clause -> the nodes/clauses that are directly behind it.
  if a << b then b maps to a
*)
open Batteries;;
open Hashtbl;;

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

(* module Clause_set = Set.Make(annotated_clause);; *)

(* endpoints of a single edge *)
type wddpac_edge =
  | Wddpac_edge of annotated_clause * annotated_clause
[@@deriving ord, to_yojson]
;;

module Wddpac_edge =
struct
  type t = wddpac_edge
  let compare = compare_wddpac_edge
  (* let pp = pp_wddpac_edge *)
  (* let to_yojson = wddpac_edge_to_yojson *)
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  type wddpac_graph

  val empty : wddpac_graph

  val add_edge : wddpac_edge -> wddpac_graph -> wddpac_graph

  val edges_of : wddpac_graph -> wddpac_edge Enum.t

  val has_edge : wddpac_edge -> wddpac_graph -> bool

  val edges_from : annotated_clause -> wddpac_graph -> wddpac_edge Enum.t

  val edges_to : annotated_clause -> wddpac_graph -> wddpac_edge Enum.t

  val preds : annotated_clause -> wddpac_graph -> annotated_clause Enum.t

  val succs : annotated_clause -> wddpac_graph -> annotated_clause Enum.t

  (* val to_yojson : wddpac_graph -> Yojson.Safe.json *)
end;;

module Graph_impl : Graph_sig =
struct
  module Wddpac_edge_set =
  struct
    module Impl = Set.Make(Wddpac_edge);;
    include Impl;;
    (* include Pp_utils.Set_pp(Impl)(Wddpac_edge);; *)
    (* include Yojson_utils.Set_to_yojson(Impl)(Wddpac_edge);; *)
  end;;

  type wddpac_graph = Graph of Wddpac_edge_set.t ;;(* [@@deriving to_yojson];; *)

  let empty = Graph(Wddpac_edge_set.empty);;

  let add_edge edge (Graph(s)) = Graph(Wddpac_edge_set.add edge s);;

  let edges_of (Graph(s)) = Wddpac_edge_set.enum s;;

  let has_edge edge (Graph(s)) = Wddpac_edge_set.mem edge s;;

  let edges_from acl (Graph(s)) =
    Wddpac_edge_set.enum s
    |> Enum.filter (fun (Wddpac_edge(acl',_)) -> equal_annotated_clause acl acl')
  ;;

  let edges_to acl (Graph(s)) =
    Wddpac_edge_set.enum s
    |> Enum.filter (fun (Wddpac_edge(_,acl')) -> equal_annotated_clause acl acl')
  ;;

  (* returns an enumeration of other annotated_clauses that have edges to acl *)
  let preds acl g =
    edges_to acl g |> Enum.map (fun (Wddpac_edge(acl,_)) -> acl)
  ;;

  (* returns an enumeration of other annotated_clauses that have edges from acl *)
  let succs acl g =
    edges_from acl g |> Enum.map (fun (Wddpac_edge(_,acl)) -> acl)
  ;;

  (* let to_yojson = wddpac_graph_to_yojson;; *)
end;;

include Graph_impl;;
