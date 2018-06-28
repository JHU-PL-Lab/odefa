open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Pp_utils;;
open Unbounded_context_stack_naive;;
open Lookup_stack;;
open Wddpac_graph_original;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

type evaluation_environment = value Environment.t;;

let pp_evaluation_environment = pp_map pp_var pp_value Environment.enum;;
let show_evaluation_environment = pp_to_string pp_evaluation_environment;;

exception Evaluation_failure of string;;

(* returns the last program point of the program *)
let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last body in x
;;

(**
   Adds a set of edges to the DDPA graph.  This implicitly adds the vertices
   involved in those edges.  Note that this does not affect the end-of-block
   map.
*)
let add_edges edges_in graph =
  (* filer out edges already in the graph *)
  let edges =
    edges_in
    |> Enum.filter
      (fun edge -> not @@ Wddpac_graph_original.has_edge edge graph)
  in
  if Enum.is_empty edges then graph else
    (* ***
       Add the edge to the DDPA graph.
    *)
    let ddpa_graph' =
      Enum.clone edges
      |> Enum.fold (flip Wddpac_graph_original.add_edge) graph
    in
    ddpa_graph'
;;

let initialize_graph cls =
  (* Create empty graph *)
  let empty_graph = Wddpac_graph_original.empty in

  (* Create beginning of graph from cls *)
  let rx = rv cls in
  let acls =
      List.enum cls
      |> Enum.map (fun x -> Unannotated_clause x)
      |> Enum.append (Enum.singleton (Start_clause rx))
      |> flip Enum.append (Enum.singleton (End_clause rx))
    in
  let rec mk_edges acls' =
      match Enum.get acls' with
      | None -> []
      | Some acl1 ->
        match Enum.peek acls' with
        | None -> []
        | Some acl2 ->
          Wddpac_edge(acl1,acl2) :: mk_edges acls'
    in
  let edges = List.enum @@ mk_edges acls in
  (* Add edges *)
  (add_edges edges empty_graph, (Start_clause(rx)))
;;

let wire site_cl func x1 x2 graph =
  let site_acl = Unannotated_clause(site_cl) in
  let Function_value(x0, Expr(body)) = func in
  let wire_in_acl = Enter_clause(x0,x1,site_cl) in
  let start_acl = Start_clause (rv body) in
  let end_acl = End_clause (rv body) in
  let wire_out_acl = Exit_clause(x2,rv body,site_cl) in
  let pred_edges =
    Wddpac_graph_original.preds site_acl graph
    |> Enum.map (fun acl' -> Wddpac_edge(acl',wire_in_acl))
  in
  let succ_edges =
    Wddpac_graph_original.succs site_acl graph
    |> Enum.map (fun acl' -> Wddpac_edge(wire_out_acl,acl'))
  in
  let inner_edges =
    List.enum body
    |> Enum.map (fun cl -> Unannotated_clause(cl))
    |> Enum.append (Enum.singleton start_acl)
    |> Enum.append (Enum.singleton wire_in_acl)
    |> flip Enum.append (Enum.singleton end_acl)
    |> flip Enum.append (Enum.singleton wire_out_acl)
    |> Utils.pairwise_enum_fold
      (fun acl1 acl2 -> Wddpac_edge(acl1,acl2))
  in
  (Enum.append pred_edges @@ Enum.append inner_edges succ_edges, start_acl)
;;


let rec lookup graph var node lookup_stack context_stack =
  (* recurisvely lookup, utilize context_stack for alignment *)
  (* Context stack isn't mutable, thank god *)
  let preds = Wddpac_graph_original.preds node graph in
  print_endline ("Var " ^ (show_var var));
  match node with
  | Unannotated_clause(Clause(x, cl)) ->
    if x <> var then (
      print_string "Skip";
      traverse_predecessors preds graph var lookup_stack context_stack
    )
    else
      begin
        match cl with
        | Var_body(x') ->
          print_string "Alias";
          traverse_predecessors preds graph x' lookup_stack context_stack
        | Value_body(Value_function(_) as v) ->
          let popped_val = Lookup_Stack.get_top lookup_stack in
          begin
            match popped_val with
            | None ->
              print_string "Value Discovery";
              Some(v)
            | Some(v) ->
              print_string "Value Discard";
              traverse_predecessors preds graph v (Lookup_Stack.pop lookup_stack) context_stack
          end
        | Appl_body(_, _) ->
          print_string "Incorrect Appl Search";
          None
        | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause"
      end

  | Enter_clause(x, x', (Clause(_, cl) as c)) ->
    if Unbounded_Stack.is_top c context_stack then
      if x = var then (
        print_string "Function enter parameter, pop from context";
        traverse_predecessors preds graph x' lookup_stack (Unbounded_Stack.pop context_stack)
      )
      else
        begin
          match cl with
          | Appl_body(xf, _) ->
            print_string "Function enter non-local, push to lookup and pop from context";
            traverse_predecessors preds graph xf (Lookup_Stack.push x lookup_stack) (Unbounded_Stack.pop context_stack)
          | _ -> raise @@ Utils.Invariant_failure "Invalid clause in enter_clause"
        end
    else (
      print_string "Misaligned Enter Clause";
      None
    )

  | Exit_clause(x, _, (Clause(_, cl) as c)) ->
    if x = var then
    begin
      print_string "Function exit";
      match cl with
      | Appl_body(xf, _) ->
        print_string "Lookup function definition";
        let fn = lookup graph xf (Unannotated_clause(c)) Lookup_Stack.empty context_stack in
        begin
          match fn with
        | Some(Value_function(Function_value(_, Expr(e)))) ->
          print_string "Get return value";
          let x' = rv e in
          print_string "Continue to traverse, pop from context stack";
          traverse_predecessors preds graph x' lookup_stack (Unbounded_Stack.push c context_stack)
        | _ -> raise @@ Utils.Invariant_failure "Found no or incorrect definitions for function"
        end
      | _ -> raise @@ Utils.Invariant_failure "Invalid clause in Exit_clause"
    end
    else (
      print_string "Skip case in exit clause";
      None
    )

  | Start_clause(_) ->
    traverse_predecessors preds graph var lookup_stack context_stack
  | End_clause(_) ->
    traverse_predecessors preds graph var lookup_stack context_stack

and traverse_predecessors preds graph var lookup_stack context_stack =
  let possible_graphs =  preds
    |> Enum.map (fun pred -> lookup graph var pred lookup_stack context_stack)
    |> Enum.filter (fun (fn) ->
        begin
          match fn with
          | None -> false
          | Some(_) -> true
        end
      )
  in
    if (Enum.count possible_graphs <> 1) then raise @@ Utils.Invariant_failure "Found multiple values for variable"
    else List.hd (List.of_enum possible_graphs)
;;

let rec create_graph graph node context_stack =
  let succs = Wddpac_graph_original.succs node graph in
  match node with
  | Unannotated_clause(Clause(x1, cl) as sitecl) ->
    begin
      match cl with
      | Var_body(_) ->
        print_string "Var body";
        traverse_succesors succs graph context_stack
      | Value_body(Value_function(_)) ->
        print_string "Value body";
        traverse_succesors succs graph context_stack
      | Appl_body(x2, x3) ->
        print_string "Appl body";
        let lookup_stack = Lookup_Stack.empty in
        print_string "Lookup fn";
        let fn = lookup graph x2 node lookup_stack context_stack in
        print_string "Lookup v";
        let v = lookup graph x3 node lookup_stack context_stack in
        (* get edges and start_clause edge from wiring in f *)
        begin
          match fn, v with
          | _, None -> raise @@ Utils.Invariant_failure "Could not find definition of value"
          | Some(Value_function(Function_value(_) as fn)), _ ->
            print_string "wire in clause";
            let edges, start_clause = wire sitecl fn x3 x1 graph in
            (* call add_edges on the edges, call traverse on enter_clause and push to context_stack *)
            print_string "Continue after adding edges and updating stack";
            create_graph ( (add_edges edges graph)) start_clause (Unbounded_Stack.push sitecl context_stack)
          | _, _ -> raise @@ Utils.Invariant_failure "Incorrect type of function"
        end
      | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause"
    end

  | Enter_clause(_, _, _) ->
    print_string "Enter clause wrong";
    (graph, false)
  | Exit_clause(_, _, c) ->
    if Unbounded_Stack.is_top c context_stack then (
      print_string "Exit clause";
      traverse_succesors succs graph (Unbounded_Stack.pop context_stack)
    )
    else (
      print_string "Exit_clause wrong";
      (graph, false)
    )

  | Start_clause(_) ->
    print_string "Start_clause";
    traverse_succesors succs graph context_stack
  | End_clause(_) ->
    print_string "End_clause";
    if Enum.is_empty succs && Unbounded_Stack.is_empty context_stack
      then (
        print_string "No successors and stack empty";
        (graph, true)
      )
    else (
      print_string "Continue through successors";
      traverse_succesors succs graph context_stack
    )

and traverse_succesors succs graph context_stack =
  let possible_graphs =  succs
    |> Enum.map (fun succ -> create_graph graph succ context_stack)
    |> Enum.filter (fun (_, valid) -> valid)
  in
    if (Enum.count possible_graphs <> 1) then raise @@ Utils.Invariant_failure "invalid graph at the end"
    else List.hd (List.of_enum possible_graphs)
;;


let eval (Expr(cls)) : value =
  let context_stack = Unbounded_Stack.empty in
  let lookup_stack = Lookup_Stack.empty in
  let initial_graph, initial_node = initialize_graph cls in
  let complete_graph, valid = create_graph initial_graph initial_node context_stack in
  if not valid then raise @@ Utils.Invariant_failure "invalid graph at the end" else
  let v = lookup complete_graph (rv cls) (End_clause(rv cls)) lookup_stack context_stack in
  match v with
  | None -> raise @@ Utils.Invariant_failure "Unable to evaluate"
  | Some(v) -> v
;;
