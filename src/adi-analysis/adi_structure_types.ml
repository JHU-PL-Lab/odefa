(** This module defines the types of structures used in the ADI
    implementation. *)

open Batteries;;
open Jhupllib;;
open Odefa_ast;;

open Adi_types;;
open Ast;;
open Ast_pp;;

module type Sig =
sig
  module S : Specification
  type address = Address of Ident.t * S.C.t
  val compare_address : address -> address -> int
  val pp_address : address Pp_utils.pretty_printer
  type environment = address Ident_map.t
  type abstract_value =
    | Abstract_int
    | Abstract_string
    | Abstract_bool of bool
    | Abstract_record of Ident.t Ident_map.t * environment
    | Abstract_function of function_value * environment
  val pp_abstract_value : abstract_value Pp_utils.pretty_printer
  module Abstract_value_set : sig
    include Set.S with type elt = abstract_value
    val pp : t Pp_utils.pretty_printer
  end;;
  module Store : sig
    include Multimap.Multimap_sig
      with type key = address and type value = abstract_value
    val pp : t Jhupllib.Pp_utils.pretty_printer
    val show : t -> string
  end
end;;

module Make(S : Specification) : Sig with module S = S =
struct
  module S = S;;

  type address =
      Address of Ident.t * S.C.t
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Address = struct
    type t = address [@@deriving eq, ord, show, to_yojson];;
  end;;

  type environment =
    Address.t Ident_map.t
  [@@deriving eq, ord, show, to_yojson]
  ;;

  type abstract_value =
    | Abstract_int
    | Abstract_string
    | Abstract_bool of bool
    | Abstract_record of Ident.t Ident_map.t * environment
    | Abstract_function of function_value * environment
  [@@deriving eq, ord, show, to_yojson]
  ;;

  module Abstract_value = struct
    type t = abstract_value [@@deriving eq, ord, show, to_yojson]
    let _ = equal;;
    let _ = show;
  end;;

  module Abstract_value_set = struct
    module Impl = Set.Make(Abstract_value);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(Abstract_value);;
  end;;

  module Store = struct
    module Impl = Multimap.Make(Address)(Abstract_value);;
    include Impl;;
    include Multimap_pp.Make(Impl)(Address)(Abstract_value);;
    include Multimap_to_yojson.Make(Impl)(Address)(Abstract_value);;
  end
end;;