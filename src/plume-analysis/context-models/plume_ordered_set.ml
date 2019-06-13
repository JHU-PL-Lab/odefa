open Batteries;;
open Jhupllib;;

open Odefa_abstract_ast;;
open Abstract_ast;;
open Plume_context_model;;
open Pp_utils;;

module Ordered_Set : Context_model =
struct
  type t = S of (abstract_clause list * Abs_clause_set.t);;
  let equal x y = compare x y == 0;;
  let compare = compare;;
  let empty = S([],Abs_clause_set.empty);;
  let push c (S(c_list,c_set)) =
    if Abs_clause_set.mem c c_set
    then S( c :: (List.filter (fun c' -> c <> c') c_list)
          (* TODO: if something goes wrong, we should check this statement *)
          , c_set)
    else S(c :: c_list, Abs_clause_set.add c c_set)
  ;;
  let pp formatter (S(c_list,_)) =
    c_list
    |> List.iter ((pp_suffix pp_var_of_abstract_clause "|") formatter);
    Format.pp_print_string formatter "?"
  ;;
  let show = pp_to_string pp;;
  let to_yojson (S(c_list,_)) =
    `List (List.map abstract_clause_to_yojson c_list)
  ;;
  let name = "osplume";;
end;;
