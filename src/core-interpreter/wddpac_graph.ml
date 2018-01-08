(**
   A module defining data structures and basic operations to form a DDPA graph.
*)

open Batteries;;

open Core_ast;;

type annotated_clause =
  | Unannotated_clause of clause
  | Enter_clause of var * var * clause
  | Exit_clause of var * var * clause
  | Start_clause of var option
  (** This variable is the return variable of the block that this clause
      starts. *)
  | End_clause of var
  (** This variable is the return variable of the block that this clause
      ends. *)
  [@@deriving ord, eq, to_yojson]
;;


type wddpac_edge =
  | Wddpac_edge of annotated_clause * annotated_clause
  [@@deriving ord, to_yojson]
;;

module Annotated_Clause = 
struct
  type t = annotated_clause
  let equal = equal_annotated_clause
  let hash = Hashtbl.hash
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

  val add_pred : wddpac_edge -> wddpac_graph -> unit

  val add_edge : wddpac_edge -> wddpac_graph -> wddpac_graph

  val has_edge : annotated_clause -> wddpac_graph -> bool

  val get_neighbor : annotated_clause -> wddpac_graph -> annotated_clause

  val get_neighbor_option : annotated_clause -> wddpac_graph -> annotated_clause option

end;;

module Graph_impl : Graph_sig =
struct

  module Wddpac_edge_tbl = Hashtbl.Make(Annotated_Clause)

  type wddpac_graph = Graph of annotated_clause Wddpac_edge_tbl.t;;

  let empty = Graph(Wddpac_edge_tbl.create 10);;

  let add_pred (Wddpac_edge(edge1,edge2)) (Graph(g)) = 
    match edge1, edge2 with
    | _, Start_clause(_) -> ()
    | Exit_clause(_,_,_), _ -> ()
    | _,_ -> Wddpac_edge_tbl.add g edge2 edge1;;

  let add_edge edge g =
    add_pred edge g;
    g;;

  let has_edge edge (Graph(g)) = 
    match edge with
    | Start_clause(_) -> true
    | _ -> Wddpac_edge_tbl.mem g edge;; 

  let get_neighbor edge (Graph(g)) = Wddpac_edge_tbl.find g edge;; 

  let get_neighbor_option edge (Graph(g)) = Wddpac_edge_tbl.find_option g edge;; 

end;;

include Graph_impl;;
