open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_ast_pp;;
open Pp_utils;;
open Unbounded_context_stack;;
open Lookup_stack;;
open Wddpac_graph;;

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

type evaluation_environment = value Environment.t;;

let pp_evaluation_environment = pp_map pp_var pp_value Environment.enum;;
let show_evaluation_environment = pp_to_string pp_evaluation_environment;;

exception Evaluation_failure of string;;

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
  let edges =
    edges_in
    |> Enum.filter
      (fun (Wddpac_edge(edge1, edge2)) -> not ((Wddpac_graph.has_succ edge1 graph) && (Wddpac_graph.has_pred edge2 graph)))
  in
  if Enum.is_empty edges then graph else
    (* ***
       Add the edges to the DDPA graph.
    *)
    let ddpa_graph' =
      Enum.clone edges
      |> Enum.fold (flip Wddpac_graph.add_edge) graph
    in
    ddpa_graph'
;;

let initialize_graph cls = 
  (* Create empty graph *)
  let empty_graph = Wddpac_graph.empty in

  (* Create beginning of graph from cls *)
  let rx = rv cls in
  let edges =
      List.enum cls
      |> Enum.map (fun x -> Unannotated_clause x)
      |> Enum.append (Enum.singleton (Start_clause(None)))
      |> flip Enum.append (Enum.singleton (End_clause rx))
      |> Utils.pairwise_enum_fold
        (fun acl1 acl2 -> Wddpac_edge(acl1,acl2))
    in 
  (add_edges edges empty_graph, (End_clause rx))
;;

let wire site_cl func x1 x2 graph =
  let site_acl = Unannotated_clause(site_cl) in
  let Function_value(x0, Expr(body)) = func in
  let wire_in_acl = Enter_clause(x0,x1,site_cl) in
  let wire_out_acl = Exit_clause(x2,rv body,site_cl) in
  if Wddpac_graph.has_succ wire_in_acl graph then (
      (* print_endline "Wire in acl already exists"; *)
      (List.enum [], wire_out_acl)
    )
  else 
    let start_acl = Start_clause(Some(x0)) in
    let end_acl = End_clause (rv body) in
    let pred_acl = Wddpac_graph.direct_pred site_acl graph
    in
    let succ_acl = Wddpac_graph.direct_succ site_acl graph
    in
    let edges =
      List.enum body
      |> Enum.map (fun cl -> Unannotated_clause(cl))
      |> Enum.append (Enum.singleton start_acl)
      |> Enum.append (Enum.singleton wire_in_acl)
      |> Enum.append (Enum.singleton pred_acl)
      |> flip Enum.append (Enum.singleton end_acl)
      |> flip Enum.append (Enum.singleton wire_out_acl)
      |> flip Enum.append (Enum.singleton succ_acl)
      |> Utils.pairwise_enum_fold
        (fun acl1 acl2 -> Wddpac_edge(acl1,acl2))
    in
    (edges, wire_out_acl)
;;

let rec lookup graph var node lookup_stack context_stack = 

  let pred_option = Wddpac_graph.direct_pred_option node graph in
  print_endline ("Var " ^ (show_var var));
  match node, pred_option with
  
  | Unannotated_clause(Clause(x, cl) as c), Some(pred) -> 
    (* print_endline "Unannotated_clause"; *)
    if x <> var then (
      (* print_endline "Skip"; *)
      lookup graph var pred lookup_stack context_stack
    )
    else
      begin
        match cl with
        | Var_body(x') -> 
          (* print_endline "Alias";  *)
          lookup graph x' pred lookup_stack context_stack
        | Value_body(Value_function(_) as v) -> 
          let popped_val = Lookup_Stack.top lookup_stack in 
          begin
            match popped_val with
            | None -> 
              (* print_endline "Value Discovery"; *)
              v
            | Some(x1) -> 
              (* print_endline "Value Discard"; *)
              lookup graph x1 pred (Lookup_Stack.pop lookup_stack) context_stack
          end
        | Appl_body(xf, xv) -> 
          (* print_endline "Lookup function"; *)
          let fn = lookup graph xf pred Lookup_Stack.empty context_stack in
          
          begin
            match fn with
            | Value_function(Function_value(_, Expr(_)) as fn) ->
                let edges, exit_clause = wire c fn xv x graph in
                (* print_endline "Lookup given function and return value"; *)
                lookup (add_edges edges graph) var exit_clause lookup_stack context_stack
            | _ -> raise @@ Utils.Invariant_failure "Found incorrect definitions for function"
          end
        | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause"
      end

  | Enter_clause(x, x', (Clause(_, cl))), Some(pred) -> 
    (* print_endline "enter_clause "; *)
      if x = var then (
        (* print_endline "Function enter parameter, pop from context"; *)
        lookup graph x' pred lookup_stack (Unbounded_Stack.pop context_stack)
      )
      else 
        begin
          match cl with 
          | Appl_body(xf, _) -> 
            (* print_endline "Function enter non-local, push to lookup and pop from context"; *)
            lookup graph xf pred (Lookup_Stack.push var lookup_stack) (Unbounded_Stack.pop context_stack)
          | _ -> raise @@ Utils.Invariant_failure "Invalid clause in enter_clause"
        end

  | Exit_clause(_, x', c), Some(pred) -> 
    (* print_endline "Exit_clause "; *)
    lookup graph x' pred lookup_stack (Unbounded_Stack.push c context_stack)
  | Exit_clause(_, _, _), None -> 
    (* print_endline "Exit_clause None"; *)
    raise @@ Utils.Invariant_failure "Found no definitions for variable"
  | Start_clause(None), None -> 
    (* print_endline "Start_clause None "; *)
    raise @@ Utils.Invariant_failure "Found no definitions for variable"
    
  | Start_clause(Some(x0)), None ->
    (* print_endline "Start_clause Some "; *)
    begin
      match Unbounded_Stack.top context_stack with
      | Some(Clause(_,Appl_body(_,xv)) as c) -> lookup graph var (Enter_clause(x0,xv,c)) lookup_stack context_stack
      | _ -> raise @@ Utils.Invariant_failure "Incorrect context stack"
    end
  | End_clause(_), Some(pred) -> 
    (* print_endline "End_clause";   *)
    lookup graph var pred lookup_stack context_stack 
  | _, _ -> raise @@ Utils.Invariant_failure "Could not find valid predecessor node"
;;

let eval (Expr(cls)) =
  let context_stack = Unbounded_Stack.empty in
  let lookup_stack = Lookup_Stack.empty in
  let initial_graph, final_node = initialize_graph cls in 
  lookup initial_graph (rv cls) final_node lookup_stack context_stack;
;;
