(* file has string conversion methods and the annotated_clause type needed for the cfg construction *)
open Jhupllib;;
open Batteries;;

open Core_ast;;
open Core_ast_pp;;
open Pp_utils;;

module Environment = Var_hashtbl;;
type evaluation_environment = value Environment.t;;
let pp_evaluation_environment = pp_map pp_var pp_value Environment.enum;;
let show_evaluation_environment = pp_to_string pp_evaluation_environment;;

module Iota = Var_hashtbl;;
type input_mapping = value Iota.t;;
let pp_input_mapping = pp_map pp_var pp_value Iota.enum;;
let show_input_mapping = pp_to_string pp_input_mapping;;

module Annotated_Clause =
struct
  type t = var
  let equal = equal_var
  let hash = Hashtbl.hash
end;;

type annotated_clause =
  | Unannotated_clause of clause
  | Enter_clause of var * var * clause
  | Exit_clause of var * var * clause
  (* arg, pattern, then, else *)
  | Conditional_enter_clause of Core_ast.var * Core_ast.pattern * annotated_clause * annotated_clause
  (* Conditional_enter_clause, x from x = conditional *)
  | Conditional_exit_clause of Core_ast.var * Core_ast.pattern * annotated_clause * annotated_clause * annotated_clause * Core_ast.var
  | Start_clause of Core_ast.var
  | End_clause of Core_ast.var
  | Junk_clause
[@@deriving ord, eq, to_yojson]
;;

type program_state = Done of (value * Formula_type.t_phi) |
                     Working of ((var * (clause Stack.t)) Stack.t * annotated_clause * clause Stack.t * (annotated_clause,annotated_clause) Hashtbl.t * Formula_type.t_phi) |
                     Double_working of (program_state * program_state);;

let string_of_var v : string =
  let Var(i, _) = v in
  let Ident(s) = i in
  s
;;

let rec string_of_map list_of_map : string =
  match list_of_map with
  | [] ->
    ""
  | head :: tail ->
    let (key, value) = head in
    let Ident(a) = key in
    if List.is_empty tail then
      a ^ "=" ^ (string_of_var value) ^ (string_of_map tail)
    else
      a ^ "=" ^ (string_of_var value) ^ ", " ^ (string_of_map tail)
;;

let string_of_value v : string =
  match v with
  | Value_int(i) ->
    string_of_int i
  | Value_bool(b) ->
    if b then "true" else "false"
  | Value_function(Function_value(f, _)) ->
    "Function " ^ (string_of_var f)
  | Value_record(Record_value(map)) ->
    "{" ^ (string_of_map (Ident_map.bindings map)) ^ "}"
  | _ ->
    "some other value"
;;

let rec string_of_annotated_clause cl : string =
  match cl with
  | Unannotated_clause(Clause(x,body)) ->
    string_of_var x ^ " = " ^
    begin
      match body with
      | Value_body(v) -> string_of_value v
      | Var_body(v) -> "variable " ^ string_of_var v
      | Input -> "input"
      | Appl_body(v1,v2) -> "fcn " ^ string_of_var v1 ^ " with arg " ^ string_of_var v2
      | Conditional_body(x, _, Function_value(f1, _), Function_value(f2, _)) ->
        "conditional " ^ (string_of_var x) ^ " ~ p ? " ^ (string_of_var f1) ^ " : " ^ (string_of_var f2)
      | Unary_operation_body(op, v) ->
        "unary " ^ (show_unary_operator op) ^ (string_of_var v)
      | Binary_operation_body(v1, op, v2) ->
        (string_of_var v1) ^ " " ^ (show_binary_operator op) ^ " " ^ (string_of_var v2)
      | _ -> "some other body"
    end
  | Enter_clause(param, arg, context_clause) ->
    "Enter clause " ^ (string_of_var param) ^ " = " ^ (string_of_var arg) ^ " C: " ^ string_of_annotated_clause (Unannotated_clause(context_clause))
  | Exit_clause(original_program_point, new_program_point, context_clause) ->
    "Exit clause " ^ (string_of_var original_program_point) ^ " = " ^ (string_of_var new_program_point) ^ " C: " ^ string_of_annotated_clause (Unannotated_clause(context_clause))
  | Conditional_enter_clause(_) ->
    "Conditional_enter_clause"
  | Conditional_exit_clause(_) ->
    "Conditional_exit_clause"
  | Start_clause(v) ->
    "Start clause " ^ (string_of_var v)
  | End_clause(v) ->
    "End clause " ^ (string_of_var v)
  | Junk_clause ->
    "Junk clause"
;;

let rec string_of_input_mapping_helper lst : string =
  match lst with
  | [] -> ""
  | (k, v) :: tail ->
    (string_of_var k) ^ " -> " ^ (string_of_value v) ^ ", " ^ (string_of_input_mapping_helper tail)
;;

let string_of_input_mapping (iota:input_mapping) : string =
  let lst_of_iota = BatList.of_enum (Iota.enum iota) in
  "{" ^ (string_of_input_mapping_helper lst_of_iota) ^ "}"
;;

let string_of_clause_body (body:clause_body) : string =
  match body with
  | Value_body(v) -> string_of_value v
  | Var_body(v) -> string_of_var v
  | Appl_body(_,x) -> "Function " ^ (string_of_var x)
  | Conditional_body(_,_,_,_) -> "conditional"
  | Projection_body(_,_) -> "projection"
  | Deref_body(_) -> "deref"
  | Update_body(_,_)-> "update"
  | Binary_operation_body(v1,op,v2) ->
    begin
      match op with
      | Binary_operator_plus ->
        string_of_var v1 ^ " + " ^ string_of_var v2
      | Binary_operator_int_minus ->
        string_of_var v1 ^ " - " ^ string_of_var v2
      | Binary_operator_int_less_than ->
        string_of_var v1 ^ " < " ^ string_of_var v2
      | Binary_operator_int_less_than_or_equal_to ->
        string_of_var v1 ^ " <= " ^ string_of_var v2
      | Binary_operator_equal_to ->
        string_of_var v1 ^ " == " ^ string_of_var v2
      | Binary_operator_bool_and ->
        "And(" ^ string_of_var v1 ^ ", " ^ string_of_var v2 ^ ")"
      | Binary_operator_bool_or ->
        "Or(" ^ string_of_var v1 ^ ", " ^ string_of_var v2 ^ ")"
      | Binary_operator_index ->
        string_of_var v1 ^ " . " ^ string_of_var v2
      | Binary_operator_tilde ->
        string_of_var v1 ^ " ~ " ^ string_of_var v2
    end
  | Unary_operation_body(op,v) ->
    begin
      match op with
      | Unary_operator_bool_not ->  "not" ^ (string_of_var v)
      | Unary_operator_bool_coin_flip ->  "coin_flip" ^ (string_of_var v)
    end
  | Input -> "Input"
;;

let rec string_of_context_stack context_stack : string =
  let stack_copy = Stack.copy context_stack in
  if Stack.is_empty stack_copy then
    ""
  else
    let Clause(x, _) = Stack.pop stack_copy in
    (string_of_var x) ^ "I" ^ (string_of_context_stack stack_copy)
;;

(* formula to string converter *)
let rec string_of_formula_2 formula : string =
  match formula with
  | Binary_formula(f1, op, f2) ->
    begin
      match op with
      | Binary_operator_plus ->
        "( " ^ string_of_formula_2 f1 ^ " + " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_int_minus ->
        "( " ^ string_of_formula_2 f1 ^ " - " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_int_less_than ->
        "( " ^ string_of_formula_2 f1 ^ " < " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_int_less_than_or_equal_to ->
        "( " ^ string_of_formula_2 f1 ^ " <= " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_equal_to ->
        "( " ^ string_of_formula_2 f1 ^ " == " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_bool_and ->
        "And (" ^ string_of_formula_2 f1 ^ ", " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_bool_or ->
        "Or (" ^ string_of_formula_2 f1 ^ ", " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_index ->
        "( " ^ string_of_formula_2 f1 ^ " . " ^ string_of_formula_2 f2 ^ " )"
      | Binary_operator_tilde ->
        "( " ^ string_of_formula_2 f1 ^ " ~ " ^ string_of_formula_2 f2 ^ " )"
    end
  | Negated_formula(f1) -> "Not (" ^ string_of_formula_2 f1 ^ ")"
  | Value_formula(v) ->
    (match v with
     | Value_record(Record_value(map)) -> "{" ^ (string_of_map (Ident_map.bindings map)) ^ "}"
     | Value_function(_) -> "function"
     | Value_ref(_) -> "ref"
     | Value_int(i) -> string_of_int i
     | Value_bool(b) ->
       begin
         match b with
         | true -> "True"
         | false -> "False"
       end
    )
  | Var_formula(var, context_stack) ->
    begin
      match var with
      | Var(i, _) ->
        begin
          match i with
          | Ident(s) -> s ^ "AAA" ^ (string_of_context_stack context_stack) ^ "AAA"
        end
    end
  | Pattern_formula(pattern) ->
    string_of_pattern pattern
;;

let rec string_of_phi phi =
  match phi with
  | [] -> ""
  | head :: tail ->
    (string_of_formula_2 head) ^ " , " ^  (string_of_phi tail)
;;

let print_phi phi =
  print_endline "Phi:";
  print_endline (string_of_phi phi)
;;

let print_set s = Set.iter print_endline s;;

let print_intermediate_map map: unit =
  print_endline "CFG precursor:";
  Hashtbl.iter (fun x -> fun y -> print_endline ((string_of_var x) ^ " -> " ^ (string_of_clause_body y))) map
;;

let print_graph graph : unit =
  print_endline "Graph:";
  Hashtbl.iter (fun x -> fun y -> print_endline ((string_of_annotated_clause x) ^ ", " ^ (string_of_annotated_clause y))) graph;
  print_endline ""
;;

let print_iota iota : unit =
  print_endline "Iota:";
  Hashtbl.iter (fun x -> fun y -> print_endline ((string_of_var x) ^ " -> " ^ (string_of_value y))) iota
;;

let print_stack stack : unit =
  print_endline "Stack:";
  Stack.iter (fun (v,_) -> print_endline ((string_of_var v))) stack
  (* Stack.iter (fun (v,f) -> print_endline ((string_of_var v) ^ " with formula: " ^ (string_of_formula f))) stack *)
;;

let print_context_stack stack : unit =
  if Stack.is_empty stack then
    print_endline "Empty context stack"
  else
    (
      print_endline "Context";
      Stack.iter (fun c -> print_endline ((string_of_annotated_clause (Unannotated_clause(c))))) stack
    )
;;

let rec string_of_program_state state : string =
  match state with
  | Done(v, phi) ->
    "Done(" ^ (string_of_value v) ^ ", " ^ (Formula_type.string_of_phi phi) ^ ")"
  | Working(l, n, c, _,_) ->
    print_stack l;
    "Working(" ^ (string_of_annotated_clause n) ^ ", " ^ (string_of_context_stack c)
  | Double_working(state1, state2) ->
    "Double_working(" ^ (string_of_program_state state1) ^ ", " ^ (string_of_program_state state2) ^ ")"
;;

let print_queue queue : unit =
  if Queue.is_empty queue then
    print_endline "Empty queue"
  else
    (
      print_endline "Queue";
      Queue.iter (fun state -> print_endline ((string_of_program_state state))) queue
    )
;;

(* find starting node in the a1 position, then create temporary junk mapping and returns new node *)
let rec create_starting_node (graph:(annotated_clause * annotated_clause) list) v g =
  match graph with
  | [] ->
    failwith "starting program point not found"
  | (a1, _) :: tail ->
    begin
      match a1 with
      | Unannotated_clause(Clause(x, _)) ->
        if x = v then
          let new_node = Junk_clause in (* new_node can be any type of clause *)
          Hashtbl.add g new_node a1;
          (new_node, g)
        else
          create_starting_node tail v g
      | Enter_clause(x, _, _)
      | Exit_clause(x, _,_) ->
        if v = x then
          (a1, g)
        else
          create_starting_node tail v g
      | Conditional_enter_clause(_) ->
        failwith "create_starting_node should not encounter this match case"
      | Conditional_exit_clause(_) ->
        failwith "create_starting_node should not encounter this match case"
      | Start_clause(_)
      | End_clause(_)
      | Junk_clause ->
        create_starting_node tail v g

    end
;;

(* try finding starting node in the a0 position and returns a1 *)
let rec find_starting_node_helper (graph:(annotated_clause * annotated_clause) list) v g =
  match graph with
  | [] ->
    (* print_endline "creation time"; *)
    create_starting_node (Hashtbl.to_list g) v g
  | (a1, a0) :: tail ->
    begin
      match a0 with
      | Unannotated_clause(Clause(x, body)) ->
        begin
          match body with
          | _ ->
            if x = v then
              (a1, g)
            else
              find_starting_node_helper tail v g
        end
      | Enter_clause(x, _, _)
      | Exit_clause(x, _,_) ->
        if v = x then
          (a1, g)
        else
          find_starting_node_helper tail v g
      | Conditional_enter_clause(_)
      | Conditional_exit_clause(_)
      | Start_clause(_)
      | End_clause(_)
      | Junk_clause ->
        find_starting_node_helper tail v g
    end
;;

let find_starting_node graph v : (annotated_clause * (annotated_clause, annotated_clause) Hashtbl.t) =
  let list_of_graph = Hashtbl.to_list graph in
  find_starting_node_helper list_of_graph v graph
;;

let find_starting_node_2 graph v : annotated_clause =
  let list_of_graph = Hashtbl.to_list graph in
  let node,_ = find_starting_node_helper list_of_graph v graph in
  node
;;

let rec find_clause_by_var_helper graph v =
  match graph with
  | [] -> failwith "didn't find clause"
  | head :: tail ->
    let (cl1, cl2) = head in
    let option1 =
      match cl1 with
      | Unannotated_clause(Clause(x,_)) as pos1 ->
        if x = v then pos1 else Junk_clause
      | _ -> Junk_clause
    in
    let option2 =
      match cl2 with
      | Unannotated_clause(Clause(x,_)) as pos2 ->
        if x = v then pos2 else Junk_clause
      | _ -> Junk_clause
    in
    match option1, option2 with
    | Junk_clause, Junk_clause -> find_clause_by_var_helper tail v
    | output, Junk_clause -> output
    | Junk_clause, output -> output
    | _,_ -> failwith "find_clause_by_var_helper result should not have happened"
;;

let find_clause_by_var graph v : annotated_clause =
  let list_of_graph = Hashtbl.to_list graph in
  find_clause_by_var_helper list_of_graph v
;;

let rec find_by_value_helper graph (cl:annotated_clause) =
  match graph with
  | [] -> failwith "couldn't find value"
  | (cl1, cl2) :: tail ->
    if cl = cl2 then
      cl1
    else
      find_by_value_helper tail cl
;;

let find_by_value (graph:(annotated_clause, annotated_clause) Hashtbl.t) (cl:annotated_clause) : annotated_clause =
  let list_of_graph = Hashtbl.to_list graph in
  find_by_value_helper list_of_graph cl
;;

let rec next_node_helper nodes cur_mapping : annotated_clause =
  match nodes with
  | [] ->
    Junk_clause (* really means no more possible options *)
  | head::tail ->
    if head = cur_mapping then
      List.hd tail
    else
      next_node_helper tail cur_mapping
;;

let next_node (graph:(annotated_clause, annotated_clause) Hashtbl.t) key cur_mapping : annotated_clause =
  let list_of_nodes = Hashtbl.find_all graph key in
  next_node_helper list_of_nodes cur_mapping
;;

let get_non_exit_clause_node (graph:(annotated_clause, annotated_clause) Hashtbl.t) key : annotated_clause =
  let list_of_nodes = Hashtbl.find_all graph key in
  List.hd (List.filter (fun a ->
      match a with
      | Exit_clause(_) ->
        false
      | _ -> true
    ) list_of_nodes)
;;

let rec matching_node_helper nodes context : annotated_clause =
  match nodes with
  | [] ->
    failwith "couldn't find any matching nodes: utils"
  | head :: tail ->
    match head with
    | Enter_clause(_,_,c)
    | Exit_clause(_,_,c) ->
      if c = context then
        head
      else
        matching_node_helper tail context
    | Conditional_enter_clause(_)
    | Conditional_exit_clause(_)
    | Unannotated_clause(_)
    | Start_clause(_)
    | End_clause(_)
    | Junk_clause ->
      matching_node_helper tail context
;;

let matching_node graph node context : annotated_clause =
  let list_of_nodes = Hashtbl.find_all graph node in
  matching_node_helper list_of_nodes context
;;

let get_exit_nodes graph node true_var : (annotated_clause * annotated_clause) =
  let candidates = Hashtbl.find_all graph node in
  let exit_clauses = List.filter (fun ac ->
      match ac with
      | Exit_clause(_) -> true
      | _ -> false
    ) candidates
  in
  match (List.hd exit_clauses) with
  | Exit_clause(_, return_var, _) ->
    if return_var = true_var then
      (List.hd exit_clauses, List.last exit_clauses)
    else
      (List.last exit_clauses, List.hd exit_clauses)
  | _ -> failwith "get_exit_nodes failure"
;;

let rec create_list_copy (list_to_copy: 'a list) : 'a list =
  match list_to_copy with
  | [] -> []
  | head :: tail ->
    head :: create_list_copy tail
;;

let clause_to_annotated_clause graph (cl:Core_ast.clause) : annotated_clause =
  let Clause(x, body) = cl in
  let candidate =
    match body with
    | Conditional_body(arg, _, Function_value(_, _), Function_value(b, _)) ->
      (
        match Hashtbl.find graph (Enter_clause(b, arg, cl)) with
        | Conditional_enter_clause(a, p, n1, n2) as w1 ->
          Conditional_exit_clause(a, p, n1, n2, w1, x)
        | _ ->
          failwith "clause_to_annotated_clause"
      )
    | _ ->
      Unannotated_clause(cl)
  in
  if Hashtbl.mem graph candidate then
    candidate
  else
    failwith ("converted clause not found: " ^ (string_of_annotated_clause candidate))
;;

let rec trim_list phi to_return =
  match phi with
  | [] -> to_return
  | head :: tail ->
    if List.mem head to_return then
      trim_list tail to_return
    else
      trim_list tail (to_return@[head])
;;


(* right now successor only works for a single input var *)
let successor iota : input_mapping =
  (* print_endline "\nsuccessor\n"; *)
  let f _ cur_value =
    let value =
      match cur_value with
      | Value_int(i) -> i
      | _ -> failwith "iota had non-integer mapping"
    in
    if value = 0 then
      Value_int(1)
    else if value < 0 then
      Value_int(value * -1 + 1)
    else
      Value_int(value * -1)
  in
  Iota.map f iota
;;

let rec evaluate_patterns formula : formula =
  (* print_endline ("\ncur formula: " ^ (string_of_formula formula)); *)
  match formula with
  | Binary_formula(f1, op, f2) ->
    begin
      match op with
      | Binary_operator_tilde ->
        (* print_endline "tilde"; *)
        let f1_value = evaluate_formula (evaluate_patterns f1) in
        (* print_endline ("f1: " ^ (string_of_formula f1) ^ " with value: " ^ (string_of_value f1_value)); *)
        (* print_endline ("f2: " ^ (string_of_formula f2)); *)
        begin
          match f1_value,f2 with
          | Value_bool(_) as v, Pattern_formula(p) ->
            let temp = matches v p in
            (* print_endline ("match value: " ^ (string_of_bool temp)); *)
            Value_formula(Value_bool(temp))
          | Value_int(_) as v, Pattern_formula(p) ->
            Value_formula(Value_bool(matches v p))
          | _, Pattern_formula(_) ->
            failwith "Evaluate patterns - not implemented yet"
          | _,_ ->
            failwith "This should never happen evaluate_patterns"
        end
      | _ ->
        (* print_endline "other binary op"; *)
        Binary_formula(evaluate_patterns f1, op, evaluate_patterns f2)
    end
  | Negated_formula(f1) ->
    (* print_endline "negated"; *)
    Negated_formula(evaluate_patterns f1)
  | Value_formula(v) ->
    (* print_endline "value"; *)
    Value_formula(v)
  | Var_formula(_) ->
    failwith "evaluate formula in core_ast should not have var formula"
  | Pattern_formula(p) ->
    (* print_endline "pattern"; *)
    Pattern_formula(p)

(* this method expects the one variable to already have been substituted for a value, so no vars *)
and evaluate_formula formula : Core_ast.value =
  let formula_without_patterns = evaluate_patterns formula in
  (* print_endline ("formula_without_patterns: " ^ (string_of_formula formula_without_patterns)); *)
  match formula_without_patterns with
  | Binary_formula(f1, op, f2) ->
    let v1 = evaluate_formula f1 in
    let v2 = evaluate_formula f2 in
    begin
      match op with
      | Binary_operator_plus ->
        begin
          match v1, v2 with
          | Value_int(i1), Value_int(i2) ->
            Value_int(i1 + i2)
          | _ ->
            raise Formula_illformed
        end
      | Binary_operator_int_minus ->
        begin
          match v1, v2 with
          | Value_int(i1), Value_int(i2) ->
            Value_int(i1 - i2)
          | _ ->
            raise Formula_illformed
        end
      | Binary_operator_int_less_than ->
        begin
          match v1, v2 with
          | Value_int(i1), Value_int(i2) ->
            Value_bool(i1 < i2)
          | _ ->
            raise Formula_illformed
        end
      | Binary_operator_int_less_than_or_equal_to ->
        begin
          match v1, v2 with
          | Value_int(i1), Value_int(i2) ->
            Value_bool(i1 <= i2)
          | _ ->
            raise Formula_illformed
        end
      | Binary_operator_equal_to ->
        begin
          match v1, v2 with
          | Value_int(i1), Value_int(i2) ->
            Value_bool(i1 = i2)
          | Value_bool(b1), Value_bool(b2) ->
            Value_bool(b1 = b2)
          | _,_ ->
            raise Formula_illformed
        end
      | Binary_operator_bool_and ->
        begin
          match v1, v2 with
          | Value_bool(b1), Value_bool(b2) ->
            Value_bool(b1 && b2)
          | _ ->
            raise Formula_illformed
        end
      | Binary_operator_bool_or ->
        begin
          match v1, v2 with
          | Value_bool(b1), Value_bool(b2) ->
            Value_bool(b1 || b2)
          | _ ->
            raise Formula_illformed
        end
      | Binary_operator_index ->
        failwith "Binary_operator_index not implemented yet - evaluate_formula"
      | Binary_operator_tilde ->
        failwith "Tilde should not be encountered"
    end
  | Negated_formula(f1) ->
    let temp = evaluate_formula f1 in
    begin
      match temp with
      | Value_bool(b) -> Value_bool(not b)
      | Value_int(_)
      | Value_function(_)
      | Value_ref(_)
      | Value_record(_) ->
        raise Formula_illformed
    end
  | Value_formula(v) ->
    v
  | Var_formula(_) ->
    failwith "evaluate formula in utils should not have var formula"
  | Pattern_formula(_) ->
    failwith "evaluate formula in utils should not have pattern formula"
;;

(* could just return false on Formula_illformed exception. But I know that's not what is actually supposed to happen *)
let check_formula formula : bool =
  match evaluate_formula formula with
  | Value_record(_)
  | Value_function(_)
  | Value_ref(_)
  | Value_int(_) ->
    true
  | Value_bool(b) ->
    b
;;
