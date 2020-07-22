(** This module defines basic data types used by the symbolic interpreter. *)

open Batteries;;
open Jhupllib;;
open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Pp_utils;;

(** The type of a symbol in the symbolic interpreter.  This is essentially the
    type of a variable using a stack-costack pair rather than a freshening
    stack. *)
type symbol =
  | Symbol of Ident.t * Relative_stack.t
[@@deriving eq, ord, to_yojson]
;;

let pp_symbol : symbol pretty_printer =
  fun formatter symbol ->
  match symbol with
  | Symbol(x,relstack) ->
    pp_ident formatter x;
    Relative_stack.pp formatter relstack;
;;
let show_symbol = pp_to_string pp_symbol;;

module Symbol = struct
  type t = symbol [@@deriving eq, ord, show, to_yojson];;
end;;

module Symbol_set = struct
  module M = Set.Make(Symbol);;
  include M;;
  include Pp_utils.Set_pp(M)(Symbol);;
  include Yojson_utils.Set_to_yojson(M)(Symbol);;
end;;

module Symbol_map = struct
  module M = Map.Make(Symbol);;
  include M;;
  include Pp_utils.Map_pp(M)(Symbol);;
  include Yojson_utils.Map_to_yojson(M)(Symbol);;
end;;

(** Information that is associated with a type abort clause, i.e. an abort that
    is triggered upon a type error. *)
type type_abort_info = {
  (** The identifier for the abort clause. *)
  abort_ident : ident;

  (** The match clauses that constrain the type of the variables in the
      operation.  For the abort to trigger, ANY one of the matches may be
      false. *)
  abort_matches : clause Ident_map.t;

  (** The operation that the abort conditions.  If the operation fails then the
      abort is triggered. *)
  abort_operation : clause;
}
[@@ deriving eq, ord, show]
;;

(** Information that is associated with a match abort clause, i.e. an abort
    that is triggered if a variable fails a pattern match expression. *)
type match_abort_info = {
  (** The identifier for the abort clause. *)
  abort_ident : ident;

  (** The match clauses that are part of the match expression.  For the abort
      to trigger, ALL of the matches must be false. *)
  abort_matches : clause Ident_map.t;
}
[@@ deriving eq, ord, show]
;;

type abort_info =
  | Type_abort_info of type_abort_info
  | Match_abort_info of match_abort_info
[@@ deriving eq, ord, show]
;;

type abort_value = {
  abort_conditional_clauses : clause list;
  abort_predicate_idents : ident list;
  abort_return_clauses : clause list;
}
[@@ deriving eq, ord, show]
;;