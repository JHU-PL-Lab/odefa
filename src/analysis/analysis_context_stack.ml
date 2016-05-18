(** A module defining the interface of a context stack. *)

open Batteries;;

open Ddpa_graph;;

module type Context_stack =
sig
  type t
  val compare : t -> t -> int
  val empty : t
  val push : abstract_clause -> t -> t Enum.t
  val pop : t -> t Enum.t
  val is_top : abstract_clause -> t -> bool
  val pp : t -> string
  val ppa : t -> string
  val name : string
end;;
