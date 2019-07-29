(**
   A module defining data structures and basic operations to form a Plume graph.
*)

open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;

open Abstract_ast;;
open Plume_context_model;;
open Pp_utils;;

module type Edge_sig = sig
  module C : Context_model;;
  type node =
    | Node of annotated_clause * (C.t);;

  type t =
    | Edge of node * node
  ;;
  (* type t;; *)
  val compare : t -> t -> int;;
  val pp : t pretty_printer;;
  val show : t -> string;;
  val to_yojson : t -> Yojson.Safe.t;;
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  module C : Context_model

  module E : Edge_sig with module C = C

  (* module Node_set : Node_Set *)

  type t
  type edge = E.t
  type node = E.node

  val equal_node : node -> node -> bool

  val compare_node : node -> node -> int

  val pp_node : node pretty_printer

  val show_node : node -> string

  val node_to_yojson : node -> Yojson.Safe.t

  val empty : t

  val add_edge : edge -> t -> t

  val edges_of : t -> edge Enum.t

  val has_edge : edge -> t -> bool

  val edges_from : node -> t -> edge Enum.t

  val edges_to : node -> t -> edge Enum.t

  val preds : node -> t -> node Enum.t

  val succs : node -> t -> node Enum.t

  val to_yojson : t -> Yojson.Safe.t

  val pp : t pretty_printer

  val show : t -> string




end;;

(* TODO: improve the performance of this implementation! *)
module Graph_impl (C : Context_model) : Graph_sig with module C = C =
struct
  module C = C;;

  module E =
  struct
    module C = C;;
    type node =
      | Node of annotated_clause * (C.t)
    [@@deriving ord, show, to_yojson]
    ;;

    let _ = show_node;;

    type t =
      | Edge of node * node
    [@@deriving ord, show, to_yojson]
    ;;
  end;;

  open E;;

  module Node =
  struct
    type t = E.node;;
    let compare = E.compare_node;;
    let pp = E.pp_node;;
    let to_yojson = E.node_to_yojson;;
  end;;

  type node = E.node[@@deriving ord, show, to_yojson];;

  type edge = E.t;;

  let equal_node node1 node2 : bool =
    let Node(acl1, c1) = node1 in
    let Node(acl2, c2) = node2 in
    let acl_check = equal_annotated_clause acl1 acl2 in
    let c_check = C.equal c1 c2 in
    acl_check && c_check
  ;;

  module Node_edge_multimap =
  struct
    module Impl = Multimap.Make(Node)(E);;
    include Impl;;
    include Multimap_pp.Make(Impl)(Node)(E);;
    include Multimap_to_yojson.Make(Impl)(Node)(E);;
  end;;

  module Edge_set =
  struct
    module Impl = Set.Make(E);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(E);;
    include Yojson_utils.Set_to_yojson(Impl)(E);;
  end;;

  type t = {
    g_all_edges : Edge_set.t;
    g_edges_from : Node_edge_multimap.t;
    g_edges_to : Node_edge_multimap.t;
  } [@@deriving to_yojson];;

  let empty = {
    g_all_edges = Edge_set.empty;
    g_edges_from = Node_edge_multimap.empty;
    g_edges_to = Node_edge_multimap.empty;
  };;

  let add_edge edge g =
    let Edge(source,target) = edge in
    {
      g_all_edges = Edge_set.add edge g.g_all_edges;
      g_edges_from = Node_edge_multimap.add source edge g.g_edges_from;
      g_edges_to = Node_edge_multimap.add target edge g.g_edges_to;
    }
  ;;

  let edges_of g = Edge_set.enum g.g_all_edges;;

  let has_edge edge g = Edge_set.mem edge g.g_all_edges;;

  let edges_from node g = Node_edge_multimap.find node g.g_edges_from;;

  let succs node g =
    edges_from node g |> Enum.map (fun (Edge(_,node)) -> node)
  ;;

  let edges_to node g = Node_edge_multimap.find node g.g_edges_to;;

  let preds node g =
    edges_to node g |> Enum.map (fun (Edge(node, _)) -> node)
  ;;

  let pp formatter g =
    (* pp_concat_sep_delim "{" "}" ", " pp_edge formatter @@ edges_of g *)
    pp_concat_sep_delim "{" "}" ", " (E.pp) formatter @@ edges_of g
  ;;

  let show = pp_to_string pp;;

end;;
