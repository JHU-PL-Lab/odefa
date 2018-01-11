(**
   A module defining data structures and basic operations to cache lookups.
*)

open Batteries;;
open Core_ast;;

open Unbounded_context_stack;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Lookup_Table_Sig =
sig
  type lookup_table

  val empty : lookup_table

  val lookupInTable: lookup_table -> var -> context_var dq ->  (value * (var option * int) * context_var dq) option

  val add : lookup_table -> var -> context_var dq -> value -> (var option * int) -> context_var dq -> unit

end;;

let equal_var_context (x_var, x_clause_dq) (y_var, y_clause_dq) = 
  (equal_var x_var y_var) && (Unbounded_Stack.equal x_clause_dq y_clause_dq)
;;

module Var_Context = 
struct
  type t = var * context_var dq
  let equal = equal_var_context
  let hash = Hashtbl.hash
end;;

module Lookup_Table_impl : Lookup_Table_Sig =
struct

  module Lookup_tbl = Hashtbl.Make(Var_Context)

  type lookup_table = Table of (value * (var option * int) * context_var dq) Lookup_tbl.t ;;

  let empty = Table(Lookup_tbl.create 10);;

  let add (Table(t)) v c value_l cl context_stack = 
    (* Lookup_tbl.remove t (v,c); *)
     Lookup_tbl.add t (v,c) (value_l, cl, context_stack);;

  let lookupInTable (Table(t)) v c = Lookup_tbl.find_option t (v,c);; 

end;;

include Lookup_Table_impl;;
