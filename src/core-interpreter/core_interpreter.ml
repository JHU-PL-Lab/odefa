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

type evaluation_environment = string Environment.t;;

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
  let ddpa_graph' =
    Enum.clone edges_in
    |> Enum.fold (flip Wddpac_graph.add_edge) graph
  in
  ddpa_graph'
;;

let initialize_graph cls = 
  (* Create empty graph *)
  let empty_graph = Wddpac_graph.empty in
  let size = List.length cls in
  (* Create beginning of graph from cls *)
  let rx = rv cls in
  let edges =
      List.enum cls
      |> Enum.mapi (fun i -> fun (Clause(v, _) as cl) -> (v, Unannotated_clause(cl), None, i+1))
    in 
  (add_edges edges empty_graph, rx, size)
;;

let wire func graph =
  let Function_value(x0, Expr(cls)) = func in
  let size = List.length cls in
  if Wddpac_graph.has_context x0 graph then (
    (graph, rv cls, x0, size)
  )
  else 
    let edges =
        List.enum cls
        |> Enum.mapi (fun i -> fun (Clause(v, _) as cl) -> (v, Unannotated_clause(cl), Some(x0), i+1 ))
        |> Enum.append (Enum.singleton (x0, Start_clause(Some(x0)), Some(x0), 0))
      in 
    (add_edges edges graph, rv cls, x0, size)
;;

let rec lookup graph var (ctx, index) lookup_stack context_stack lookup_table = 
  
  (* Align *)
  let (node, i) = begin
    let Graph_node(n, c, i) = Wddpac_graph.lookup var graph in
    if c = ctx && i <= index then (n, i)
    else (Start_clause(ctx), 0)
  end in

  (* Process Graph Node *)
  begin
    let (v,c,c_stack) = 
      begin
        match node with
        
        | Unannotated_clause(Clause(x, cl) as c) -> 
          if x <> var then (
            raise @@ Utils.Invariant_failure ("Found no definitions for variable from exit clause 1" )
          )
          else
            begin
              match cl with
              | Var_body(x') -> 
                (* print_endline "Alias";  *)
                lookup graph x' (ctx, i) lookup_stack context_stack lookup_table
                
              | Value_body(Value_function(_) as v) -> 
                let popped_val = Lookup_Stack.top lookup_stack in 
                begin
                  match popped_val with
                  | None -> 
                    (* print_endline "Value Discovery"; *)
                    (v, (ctx, i), context_stack)
                  | Some(x1) -> 
                    (* print_endline "Value Discard"; *)
                    lookup graph x1 (ctx, i) (Lookup_Stack.pop lookup_stack) context_stack lookup_table
                end

              | Appl_body(xf, _) -> 
                (* print_endline "Lookup function"; *)
                let (fn, cfn, cfn_stack) = lookup graph xf (ctx, i) Lookup_Stack.empty context_stack lookup_table in
                (* let (_, cv, cv_stack) = lookup graph xv ctx Lookup_Stack.empty context_stack lookup_table in *)

                begin
                  match fn with
                  | Value_function(Function_value(_, Expr(_)) as fn) ->
                      let graph, rx, ctx2, size = wire fn graph in

                      Cache_lookups.add lookup_table xf context_stack cfn cfn_stack;
                      (* Cache_lookups.add lookup_table xv context_stack cv cv_stack; *)

                      (* print_endline "Exit clause"; *)
                      lookup graph rx (Some(ctx2), size) lookup_stack (Unbounded_Stack.push (Context_var(c, ctx, i)) context_stack) lookup_table

                  | _ -> raise @@ Utils.Invariant_failure "Found incorrect definitions for function 2"
                end
              | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause 3"
            end

        | Start_clause(None) -> 
          raise @@ Utils.Invariant_failure ("Found no definitions for variable 4")
          
        | Start_clause(Some(x0)) ->
          (* print_endline "Start Clause"; *)
          begin
            match Unbounded_Stack.top context_stack with
            | Some(Context_var((Clause(_,Appl_body(xf ,xv))), ctx, i)) -> 
              begin
                let popped_context_stack = Unbounded_Stack.pop context_stack in

                if x0 = var then (
                  (* print_endline "Function enter parameter"; *)
                  lookup graph xv (ctx, i) lookup_stack popped_context_stack lookup_table
                )
                else (
                  (* print_endline "Non local lookup"; *)
                  match (Cache_lookups.lookupInTable lookup_table xf popped_context_stack) with
                  | Some((ctx,i),context_stack) -> 
                    lookup graph var (ctx,i) lookup_stack context_stack lookup_table
                  | _ ->
                    lookup graph xf (ctx, i) (Lookup_Stack.push var lookup_stack) popped_context_stack lookup_table
                )
              end
            | _ -> raise @@ Utils.Invariant_failure "Incorrect context stack"
          end
      end in 
    (v,c, c_stack)
  end 
;;

let rec substitute v cl graph context_stack lookup_table env = 
  match v with
  | Value_function(Function_value(x', Expr(e))) ->
    Environment.add env x' "0";
    List.enum e
      |> Enum.map (fun (Clause(x, _)) -> Environment.add env x "0")
      |> List.of_enum
      |> fun _ -> ()
      ;
    List.enum e
      |> Enum.map (fun c -> substitute_clause c cl graph context_stack lookup_table env)
      |> List.of_enum
      |> fun _ -> ()
      ;
    ()
  | _ -> ()
and substitute_clause cl cl2 graph context_stack lookup_table env =
  match cl with 
  | Clause(_, Value_body(v)) ->
    substitute v cl2 graph context_stack lookup_table env
  | Clause(_, Var_body(x)) -> 
    process_vars (x :: []) cl2 graph context_stack lookup_table env
  | Clause(_, Appl_body(x, x')) ->
    process_vars (x :: x' :: []) cl2 graph context_stack lookup_table env
  | _ -> ()
and process_vars vars cl graph context_stack lookup_table env = 
  List.enum vars
  |> Enum.filter (fun x -> not (Environment.mem env x))
  |> Enum.map (fun x -> 
      let (v, cl, context_stack) = lookup graph x cl (Lookup_Stack.empty) context_stack lookup_table in
      print_endline (show_var x ^ " := " ^ show_value v);
      Environment.add env x "0";
      substitute v cl graph context_stack lookup_table (Environment.create 10) )
  |> List.of_enum
  |> fun _ -> ()
;;

let eval (Expr(cls)) =
  let context_stack = Unbounded_Stack.empty in
  let lookup_stack = Lookup_Stack.empty in
  let lookup_table = Cache_lookups.empty in
  let initial_graph, rx, index = initialize_graph cls in 
  let (v, cl, context_stack) = lookup initial_graph rx (None, index) lookup_stack context_stack lookup_table in
  print_endline (show_value v);
  substitute v cl initial_graph context_stack lookup_table (Environment.create 10);
  v
;;
