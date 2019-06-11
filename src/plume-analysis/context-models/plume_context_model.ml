(** A module defining the interface of a context stack. *)

open Odefa_abstract_ast;;
open Abstract_ast;;

module type Context_model =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val empty : t
  val push : abstract_clause -> t -> t
  val pp : Format.formatter -> t -> unit
  val to_yojson : t -> Yojson.Safe.json
  val show : t -> string
  val name : string
end;;
