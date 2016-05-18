open Batteries;;

open Analysis;;
open Ast;;
open Ast_pp;;
open Ddpa_graph;;

include Toploop_ddpa_types;;

(** Finds all of the call sites in the provided expression.  Returns an
    enumeration of call sites as pairs between the function variable and the
    clause representing the call site. *)
let rec find_all_call_sites (Expr cls) =
  let rec call_sites_from_function_value (Function_value(_, e)) =
    find_all_call_sites e
  in
  let rec call_sites_from_clause cl =
    match cl with
    | Clause(_, Appl_body(x2, _)) ->
      Some (Enum.singleton (x2, cl))
    | Clause(_, Value_body(Value_function(f))) ->
      Some (call_sites_from_function_value f)
    | Clause(_, Conditional_body(_, _, f1, f2)) ->
      Some (Enum.append (call_sites_from_function_value f1)
              (call_sites_from_function_value f2))
    | _ -> None
  in
  cls
  |> List.enum
  |> Enum.filter_map
    call_sites_from_clause
  |> Enum.concat
;;

let pp_inconsistency inconsistency =
  match inconsistency with
  | Application_of_non_function(x,c,v) ->
    Printf.sprintf
      "Error: call site %s has function variable %s to which a non-function value may flow: %s"
      (pp_clause c) (pp_var x) (pp_abstract_value v)
;;

(* TODO: the "none" should be handled elsewhere as a special case *)
module type Stack = Analysis_context_stack.Context_stack;;
let name_parsing_functions =
  [
    (* A function for the literally-named modules. *)
    (fun name ->
       match name with
       | "0ddpa" ->
         Some (module Analysis_unit_stack.Stack : Stack)
       | "1ddpa" ->
         Some (module Analysis_single_element_stack.Stack : Stack)
       | "2ddpa" ->
         Some (module Analysis_two_element_stack.Stack : Stack)
       | "ddpaNR" ->
         Some (module Analysis_nonrepeating_stack.Stack : Stack)
       | "none" -> None
       | _ -> raise Not_found
    )
    ;
    (* A function for parsing kDDPA *)
    (fun name ->
       if not @@ String.ends_with name "ddpa" then raise Not_found;
       let num_str = String.sub name 0 @@ String.length name - 4 in
       try
         let num = int_of_string num_str in
         let module Spec : Analysis_n_element_stack.Spec =
         struct
           let size = num
         end
         in
         let module NStack = Analysis_n_element_stack.Make(Spec) in
         Some (module NStack : Stack)
       with
       | Failure _ -> raise Not_found
    )
    ;
    (* A function for parsing CkDDPA. *)
    (fun name ->
       if not @@ String.starts_with name "c" then raise Not_found;
       if not @@ String.ends_with name "ddpa" then raise Not_found;
       let num_str = String.sub name 1 @@ String.length name - 5 in
       try
         let num = int_of_string num_str in
         let module Spec : Analysis_n_element_collapsing_stack.Spec =
         struct
           let size = num
         end
         in
         let module NStack = Analysis_n_element_collapsing_stack.Make(Spec) in
         Some (module NStack : Stack)
       with
       | Failure _ -> raise Not_found
    )
  ];;
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
;;

module Make(A : Analysis_sig) : DDPA with module C = A.C =
struct
  type analysis =
    { aref : A.ddpa_analysis ref
    ; expression : expr
    };;

  module C = A.C;;

  let create_analysis ?logging_prefix:(pfx=None) expr =
    let a = A.create_initial_analysis ~logging_prefix:pfx expr in
    { aref = ref @@ A.perform_full_closure a
    ; expression = expr
    }
  ;;

  let values_of_variable_from x acl analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.values_of_variable x acl a in
    analysis.aref := a';
    values
  ;;

  let contextual_values_of_variable_from x acl ctx analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.contextual_values_of_variable x acl ctx a in
    analysis.aref := a';
    values
  ;;

  let check_inconsistencies analysis =
    find_all_call_sites analysis.expression
    |> Enum.map
      (fun (x2, cl) ->
         let acl = Unannotated_clause(lift_clause cl) in
         let (values, a') = A.values_of_variable x2 acl !(analysis.aref) in
         analysis.aref := a';
         values
         |> Abs_filtered_value_set.enum
         |> Enum.filter_map
           (fun (Abs_filtered_value(v,_,_)) ->
              match v with
              | Abs_value_function _ -> None
              | _ -> Some (Application_of_non_function(x2, cl, v))
           )
      )
    |> Enum.concat
    |> List.of_enum (* force us to pull on the enum so the analysis
                       updates *)
    |> List.enum
  ;;

  let pp_analysis analysis =
    A.pp_ddpa !(analysis.aref)
  ;;

  let get_size analysis =
    A.get_size !(analysis.aref)
  ;;
end;;
