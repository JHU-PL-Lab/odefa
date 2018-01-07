(**
   A module defining data structures and basic operations to form a DDPA graph.
*)

open Batteries;;

open Core_ast;;

type annotated_clause =
  | Unannotated_clause of clause
  | Start_clause of var option
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
  type wddpac_graph

  val empty : wddpac_graph

  val add_edge : var * annotated_clause * var option -> wddpac_graph -> wddpac_graph

  val has_context : var -> wddpac_graph -> bool

  val lookup : var -> wddpac_graph -> (annotated_clause * var option)
  
(* 
  val has_pred : annotated_clause -> wddpac_graph -> bool

  val direct_pred : annotated_clause -> wddpac_graph -> annotated_clause

  val direct_pred_option : annotated_clause -> wddpac_graph -> annotated_clause option

  val direct_succ : annotated_clause -> wddpac_graph -> annotated_clause

  val direct_succ_option : annotated_clause -> wddpac_graph -> annotated_clause option
 *)
end;;

module Graph_impl : Graph_sig =
struct

  module Wddpac_edge_tbl = Hashtbl.Make(Annotated_Clause)

  type wddpac_graph = Graph of (annotated_clause * var option) Wddpac_edge_tbl.t;;(* [@@deriving to_yojson];; *)

  let empty = Graph(Wddpac_edge_tbl.create 10);;

  let add_edge (v, cl, context) (Graph(g)) = 
    Wddpac_edge_tbl.add g v (cl, context);
    Graph(g)
  ;;

  let has_context ctx (Graph(g)) = 
    Wddpac_edge_tbl.mem g ctx
  ;;

  let lookup var (Graph(g)) = 
    Wddpac_edge_tbl.find g var
  ;;

 (*  let has_succ edge (Graph(_,s)) = 
    match edge with
    | End_clause(_) -> true
    | _ -> Wddpac_edge_tbl.mem s edge;; 

  let has_pred edge (Graph(p,_)) = 
    match edge with
    | Start_clause(_) -> true
    | _ -> Wddpac_edge_tbl.mem p edge;; 

  let direct_succ edge (Graph(_,s)) = Wddpac_edge_tbl.find s edge;; 

  let direct_pred edge (Graph(p,_)) = Wddpac_edge_tbl.find p edge;; 

  let direct_succ_option edge (Graph(_,s)) = Wddpac_edge_tbl.find_option s edge;; 

  let direct_pred_option edge (Graph(p,_)) = Wddpac_edge_tbl.find_option p edge;;  *)

end;;

include Graph_impl;;
