open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Abstract_ast;;
open Adi_types;;
open Pp_utils;;

module Stack : Context_model =
struct
  type t = S of abstract_clause list;;
  let equal x y = compare x y == 0;;
  let compare = compare;;
  let empty = S([]);;
  let push c (S(lst)) =
    match lst with
      | [] -> S([c])
      | h::_ -> S([c;h])
  ;;
  let pp formatter (S(lst)) =
    List.iter ((pp_suffix pp_var_of_abstract_clause "|") formatter) lst;
    Format.pp_print_string formatter "?"
  ;;
  let show = pp_to_string pp;;
  let to_yojson (S(lst)) =
    `List (List.map abstract_clause_to_yojson lst)
  ;;
  let name = "2adi";;
end;;
