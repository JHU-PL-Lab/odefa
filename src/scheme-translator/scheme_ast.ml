open Batteries;;
(* open Jhupllib;; *)

type ident = Ident of string [@@deriving eq, ord, show, to_yojson];;

type atom_literal = AString of string | AInt of int;;

type sexpr = Atom of atom_literal | Group of sexpr list;;
