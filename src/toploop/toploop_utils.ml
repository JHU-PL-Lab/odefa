open Batteries;;

open Odefa_ast;;
open Odefa_ddpa;;
open Odefa_plume;;
open Odefa_abstract_ast;;

open Toploop_types;;
open Ast;;
open Abstract_ast;;

module type Stack = Ddpa_context_stack.Context_stack;;

module type Model = Plume_context_model.Context_model;;

let string_of_query (q : query) : string =
  let Query(lu_var, g_pos, c) = q in
  let LUVar(lu_var_name) = lu_var in
  let g_pos_name =
    begin
      match g_pos with
      | ProgramPoint (pp_name) -> pp_name
      | END -> "END"
    end
  in
  let c_list =
    "[" ^ (List.fold_left (fun acc -> fun var ->
        let LUVar(name) = var in
        acc ^ name
      ) "" c) ^ "]"
  in
  lu_var_name ^ "@" ^ g_pos_name ^ "@" ^  c_list
;;


let analysis_task_to_name (task : analysis_task) : string =
  match task with
  | DDPA (num) ->
    if (num = 0) then
      "0ddpa"
    else if (num = 1) then
      "1ddpa"
    else if (num = 2) then
      "2ddpa"
    else
      (string_of_int num) ^ "ddpa"
  | PLUME (num) ->
    if (num = 0) then
      "0plume"
    else if (num = 1) then
      "1plume"
    else if (num = 2) then
      "2plume"
    else
      (string_of_int num) ^ "plume"
  | SPLUME ->
    "set_plume"
  | OSKPLUME ->
    "ordered_set_keep_plume"
  | OSMPLUME ->
    "ordered_set_move_plume"
;;

let ddpa_analysis_to_stack (task : analysis_task) : (module Stack) =
  match task with
  | DDPA (num) ->
    if (num = 0) then
      (module Ddpa_unit_stack.Stack : Stack)
    else if (num = 1) then
      (module Ddpa_single_element_stack.Stack : Stack)
    else if (num = 2) then
      (module Ddpa_two_element_stack.Stack : Stack)
    else
      ( let module Spec : Ddpa_n_element_stack.Spec =
        struct
          let size = num
        end
        in
        let module NStack = Ddpa_n_element_stack.Make(Spec) in
        (module NStack : Stack)
      )
  | _ ->
    (* Since this function is only expecting DDPA analyses, throw an error *)
    raise Not_found
;;

let plume_analysis_to_stack (task : analysis_task) : (module Model) =
  match task with
  | PLUME (num) ->
    if (num = 0) then
      (module Plume_unit_stack.Stack : Model)
    else if (num = 1) then
      (module Plume_single_element_stack.Stack : Model)
    else if (num = 2) then
      (module Plume_two_element_stack.Stack : Model)
    else
      ( let module Spec : Plume_n_element_stack.Spec =
        struct
          let size = num
        end
        in
        let module NStack = Plume_n_element_stack.Make(Spec) in
        (module NStack : Model)
      )
  | SPLUME -> (module Plume_set.Set : Model)
  | OSKPLUME -> (module Plume_ordered_set_keep.Ordered_Set_Keep : Model)
  | OSMPLUME -> (module Plume_ordered_set_move.Ordered_Set_Move : Model)
  | _ ->
    (* Since this function is only expecting DDPA analyses, throw an error *)
    raise Not_found
;;


let name_parsing_functions =
  [
    (* TODO: After implementing PLUME, include PLUME *)
    (* A function for the literally-named modules. *)
    (fun name ->
       match name with
       | "0ddpa" ->
         DDPA (0)
       | "1ddpa" ->
         DDPA (1)
       | "2ddpa" ->
         DDPA (2)
       | "0plume" ->
         PLUME (0)
       | "1plume" ->
         PLUME (1)
       | "2plume" ->
         PLUME (2)
       (* | "none" -> None *)
       | "splume" ->
         SPLUME
       | "oskplume" ->
         OSKPLUME
       | "osmplume" ->
         OSMPLUME
       | _ -> raise Not_found
    )
    ;
    (* A function for parsing kddpa *)
    (fun name ->
       if not @@ String.ends_with name "ddpa" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 4 in
       try
         let num = int_of_string num_str in
         DDPA (num)
       with
       | Failure _ -> raise Not_found
    );
    (* A function for parsing kplume *)
    (fun name ->
       if not @@ String.ends_with name "plume" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 5 in
       try
         let num = int_of_string num_str in
         PLUME (num)
       with
       | Failure _ -> raise Not_found
    )
  ];;

let analysis_from_name name =
  let rec loop fns =
    match fns with
    | [] -> raise Not_found
    | fn::fns' ->
      begin
        try
          fn name
        with
        | Not_found -> loop fns'
      end
  in
  loop name_parsing_functions
;;
(*
let stack_from_name name =
  let rec loop fns =
    match fns with
    | [] -> raise Not_found
    | fn::fns' ->
      begin
        try
          fn name
        with
        | Not_found -> loop fns'
      end
  in
  loop name_parsing_functions
;; *)

(** Iterate recursively over all clauses in an expression. *)
let rec iterate_abstract_clauses (Abs_expr(acls)) =
  let top_level = List.enum acls in
  let nested = Enum.delay
      (fun () -> Enum.concat @@
        Enum.map (fun e -> Enum.delay (fun () -> iterate_abstract_clauses e)) @@
        Enum.concat @@ List.enum @@ List.map _abs_exprs_of_clause acls)
  in
  Enum.append top_level nested

and _abs_exprs_of_clause (Abs_clause(_,b)) =
  match b with
  | Abs_conditional_body(_,_,Abs_function_value(_,e1),Abs_function_value(_,e2))
    -> Enum.append (Enum.singleton e1) (Enum.singleton e2)
  | Abs_value_body(v) ->
    _abs_exprs_of_value v
  | Abs_var_body _ | Abs_appl_body _ | Abs_projection_body _ | Abs_deref_body _
  | Abs_update_body _ | Abs_binary_operation_body _
  | Abs_unary_operation_body _ -> Enum.empty ()

and _abs_exprs_of_value v =
  match v with
  | Abs_value_function(Abs_function_value(_,e)) -> Enum.singleton e
  | Abs_value_record _ | Abs_value_ref _ | Abs_value_int | Abs_value_bool _
  | Abs_value_string -> Enum.empty ()
;;

let last_var_of (Expr(cls)) =
  let Clause(x,_) = List.last cls in x
;;
