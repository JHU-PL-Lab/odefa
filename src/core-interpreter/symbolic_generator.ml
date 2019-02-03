open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_interpreter_utils;;

(* open Sys;; *)
open Unix;;
open Yojson.Basic.Util;;

(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

(*
  lookup function
*)
(* let rec lookup lookup_stack (node:annotated_clause) context_stack graph iota: (Core_ast.value * formula * input_mapping) = *)
let rec lookup _ =
    failwith "TODO"
;;

let script formula =
  execv "/usr/bin/python" [| "python";"/home/theodore/research/odefa/src/core-interpreter/test.py";formula|]
;;

let rec make_map clauses graph : (var,clause_body) Hashtbl.t =
  match clauses with
  | [] -> graph
  | Clause(var, body) :: tail ->
    Hashtbl.add graph var body;
    make_map tail graph
;;


let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t * formula * input_mapping =

  (* make dictionary *)
  let map = make_map cls (Hashtbl.create 10) in
  print_intermediate_map map;

  (*
  Parse through ddpa graph and construct graph, using map to get clause bodies
  *)
  (* get the json file *)
  let json = Yojson.Basic.from_file "ddpa_graphs.json" in
  (* let json = Yojson.Basic.from_file "book.json" in *)
  let temp = json |> member "graph" in
  let test = temp |> member "ddpa_graph" |> to_list in
  let json_of_graph = List.hd (List.tl test) in
  let elements = json_of_graph |> member "elements" |> to_list in
  let first_edge_list = to_list (List.hd elements) in
  (* the first entry in this list is Unannotated_clause *)
  let node1_list_list = to_list (List.nth first_edge_list 1) in
  (* first entry is Abs_clause *)
  let node1_list = to_list (List.nth node1_list_list 1) in
  (* Abs_var is first entry *)
  let node1_1 = to_list (List.nth node1_list 1) in
  (* Ident is first entry*)
  let ident_list = to_list (List.nth node1_1 1) in
  (* Finally get x *)
  let ident = to_string (List.nth ident_list 1) in

  let node_to_add = Hashtbl.find map (Var(Ident(ident), None)) in


  print_endline ("Ident is: " ^ ident);
  print_endline ("Clause to go with that: " ^ (string_of_clause_body node_to_add));


  print_endline ("edge size(3): " ^ (string_of_int (List.length first_edge_list)));
  print_endline ("this should be edge: " ^ (to_string (List.hd first_edge_list)));
  (* print_endline ("first edge: " ^ (to_list first_edge_list)); *)

  (* print_endline ("testing: " ^ test); *)
  print_endline ("list size: " ^ (string_of_int (List.length test)));
  print_endline ("this should be graph: " ^ (to_string (List.hd test)));
  print_endline ("Size of elements (3): " ^ (string_of_int (List.length elements)));




  let _ = lookup cls in

  let json = Yojson.Basic.from_file "ddpa_graphs.json" in
  let test = json |> member "element_type" |> to_string in
  print_endline ("TESTING: " ^ test);


  failwith "TODO"


;;
