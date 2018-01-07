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

let rec lookup graph var node lookup_stack context_stack lookup_table = 
  match (Cache_lookups.lookupInTable lookup_table var context_stack), (Lookup_Stack.top lookup_stack) with
    | Some(Clause(_,Value_body(v)) as c,context_stack), None -> (v,c,context_stack)
    | Some(cl,context_stack), Some(l) -> 
      lookup graph l (Unannotated_clause(cl)) (Lookup_Stack.pop lookup_stack) context_stack lookup_table
    | _, _ ->
      begin
        let (v,c,c_stack) = 
          begin
          let pred_option = Wddpac_graph.direct_pred_option node graph in
            match node, pred_option with
            
            | Unannotated_clause(Clause(x, cl) as c), Some(pred) -> 
              if x <> var then (
                lookup graph var pred lookup_stack context_stack lookup_table
              )
              else
                begin
                  match cl with
                  | Var_body(x') -> 
                    (* print_endline "Alias";  *)
                    lookup graph x' pred lookup_stack context_stack lookup_table
                    
                  | Value_body(Value_function(_) as v) | Value_body(Value_int(_) as v) | Value_body(Value_bool(_) as v) -> 
                    let popped_val = Lookup_Stack.top lookup_stack in 
                    begin
                      match popped_val with
                      | None -> 
                        (* print_endline "Value Discovery"; *)
                        (v, c, context_stack)
                      | Some(x1) -> 
                        (* print_endline "Value Discard"; *)
                        Cache_lookups.add lookup_table var context_stack c context_stack;
                        lookup graph x1 pred (Lookup_Stack.pop lookup_stack) context_stack lookup_table
                    end
                  | Appl_body(xf, xv) -> 
                    (* print_endline "Lookup function"; *)
                    let (fn, cfn, cfn_stack) = lookup graph xf pred Lookup_Stack.empty context_stack lookup_table in
                    (* let (_, cv, cv_stack) = lookup graph xv pred Lookup_Stack.empty context_stack lookup_table in *)

                    begin
                      match fn with
                      | Value_function(Function_value(_, Expr(_)) as fn) ->
                          let edges, exit_clause = wire c fn xv x graph in

                          Cache_lookups.add lookup_table xf context_stack cfn cfn_stack;
                          (* Cache_lookups.add lookup_table xv context_stack cv cv_stack; *)

                          lookup (add_edges edges graph) var exit_clause lookup_stack context_stack lookup_table

                      | _ -> raise @@ Utils.Invariant_failure "Found incorrect definitions for function"
                    end
                  | Binary_operation_body(x1,op,x2) ->
                    let (v1, _, _) = lookup graph x1 pred Lookup_Stack.empty context_stack lookup_table in 
                    let (v2, _, _) = lookup graph x2 pred Lookup_Stack.empty context_stack lookup_table in 
                    begin
                      match v1, op, v2 with
                      | Value_int(n1), Binary_operator_plus, Value_int(n2) -> (Value_int(n1 + n2), c, context_stack)
                      | Value_int(n1), Binary_operator_int_minus, Value_int(n2) -> (Value_int(n1 - n2), c, context_stack)
                      | Value_int(n1), Binary_operator_equal_to, Value_int(n2) -> (Value_bool(n1 = n2), c, context_stack)
                      | Value_bool(b1), Binary_operator_bool_and, Value_bool(b2) -> (Value_bool(b1 && b2), c, context_stack)
                      | Value_bool(b1), Binary_operator_bool_or, Value_bool(b2) -> (Value_bool(b1 || b2), c, context_stack)
                      | _,_,_ -> 
                       raise @@ Evaluation_failure "Incorrect binary operation"
                    end
                  | Unary_operation_body(op,x1) ->
                    let (v1, _, _) = lookup graph x1 pred Lookup_Stack.empty context_stack lookup_table in 
                    begin
                      match op, v1 with
                      | Unary_operator_bool_not, Value_bool(b1) -> (Value_bool(not b1), c, context_stack)
                      | _,_ -> 
                       raise @@ Evaluation_failure "Incorrect unary operation"
                    end
                  | _ -> raise @@ Utils.Invariant_failure "Usage of not implemented clause"
                end

            | Enter_clause(x, x', (Clause(_, cl))), Some(pred) -> 
                if x = var then (
                  (* print_endline "Function enter parameter"; *)
                  lookup graph x' pred lookup_stack (Unbounded_Stack.pop context_stack) lookup_table
                )
                else 
                  begin
                    match cl with 
                    | Appl_body(xf, _) -> 
                      (* print_endline "Function enter non-local"; *)
                      lookup graph xf pred (Lookup_Stack.push var lookup_stack) (Unbounded_Stack.pop context_stack) lookup_table
                    | _ -> raise @@ Utils.Invariant_failure "Invalid clause in enter_clause"
                  end

            | Exit_clause(_, x', c), Some(pred) -> 
              (* print_endline "Function Exit "; *)
              lookup graph x' pred lookup_stack (Unbounded_Stack.push c context_stack) lookup_table

            | Exit_clause(_,_,_), None -> 
              raise @@ Utils.Invariant_failure ("Found no definitions for variable from exit clause " )
            | Start_clause(None), None -> 
              raise @@ Utils.Invariant_failure ("Found no definitions for variable " ^ (show_var var) ^ " at start clause"^ " " ^ (Unbounded_Stack.show context_stack ))
              
            | Start_clause(Some(x0)), None ->
              begin
                match Unbounded_Stack.top context_stack with
                | Some(Clause(_,Appl_body(_,xv)) as c) -> lookup graph var (Enter_clause(x0,xv,c)) lookup_stack context_stack lookup_table
                | _ -> raise @@ Utils.Invariant_failure "Incorrect context stack"
              end
            | End_clause(_), Some(pred) -> 
              lookup graph var pred lookup_stack context_stack lookup_table
            | _, _ -> raise @@ Utils.Invariant_failure "Could not find valid predecessor node"
          end in 

          (* begin
            match (Lookup_Stack.is_empty lookup_stack) with
            | true -> Cache_lookups.add lookup_table var context_stack c c_stack
            | _ -> ()
          end; *)
          (v,c, c_stack)
        end 
;;

let rec substitute cl cl2 graph context_stack lookup_table env = 
  match cl2 with 
  | Clause(x, Value_body(Value_function(Function_value(x', Expr(e))))) ->
      Environment.add env x "0";
      Environment.add env x' "0";
      List.enum e
      |> Enum.map (fun (Clause(x, _)) -> Environment.add env x "0")
      |> List.of_enum
      |> fun _ -> ()
      ;
      List.enum e
      |> Enum.map (fun c -> substitute cl c graph context_stack lookup_table env)
      |> List.of_enum
      |> fun _ -> ()
      ;
      ()
  | Clause(_, Var_body(x)) -> if Environment.mem env x then () else (
        lookup graph x (Unannotated_clause(cl)) (Lookup_Stack.empty) context_stack lookup_table
        |> fun _ -> ()
      )

  | Clause(_, Appl_body(x, x')) ->
      let appl_vars = x :: x' :: [] in 
      List.enum appl_vars
      |> Enum.filter (fun x -> not (Environment.mem env x))
      |> Enum.map (fun x -> 
          let (_, cl, context_stack) = lookup graph x (Unannotated_clause(cl)) (Lookup_Stack.empty) context_stack lookup_table in
          print_endline (show_var x ^ " := " ^ show_clause cl);
          substitute cl cl graph context_stack lookup_table (Environment.create 0) )
      |> List.of_enum
      |> fun _ -> ()
  | _ -> ()
;;

let eval (Expr(cls)) =
  let context_stack = Unbounded_Stack.empty in
  let lookup_stack = Lookup_Stack.empty in
  let initial_graph, final_node = initialize_graph cls in 
  let lookup_table = Cache_lookups.empty in
  let (v, cl, context_stack) = lookup initial_graph (rv cls) final_node lookup_stack context_stack lookup_table in
  print_endline (show_clause cl);
  substitute cl cl initial_graph context_stack lookup_table (Environment.create 0);
  print_endline "";
  v
;;
