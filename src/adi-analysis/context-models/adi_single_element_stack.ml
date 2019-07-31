open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;

open Abstract_ast;;
open Adi_context_model;;
open Pp_utils;;

module Stack : Context_model =
struct
  type t = S of abstract_clause option;;
  let equal x y = compare x y == 0;;
  let compare = compare;;
  let empty = S(None);;
  let push c _ = S(Some(c));;
  let pp formatter x =
    match x with
    | S(Some(c)) -> (pp_suffix pp_abstract_clause "|?") formatter c
    | S(None) -> Format.pp_print_string formatter "?"
  ;;
  let show = pp_to_string pp;;
  let to_yojson c =
    `List (
      match c with
      | S(Some(c)) -> [abstract_clause_to_yojson c]
      | S(None) -> []
    )
  ;;
  let name_prefix = "1";;
end;;
