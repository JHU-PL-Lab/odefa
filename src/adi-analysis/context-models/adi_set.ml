(** A set. *)

open Jhupllib;;
open Odefa_abstract_ast;;

open Abstract_ast;;
open Adi_context_model;;
open Pp_utils;;

module Set : Context_model =
struct
  type t = S of Abs_clause_set.t;;
  let equal x y = compare x y == 0;;
  let compare = compare;;
  let empty = S(Abs_clause_set.empty);;
  let push c (S(prev_ctx)) = S(Abs_clause_set.add c prev_ctx);;
  let pp formatter (S(set)) =
    Abs_clause_set.iter ((pp_suffix pp_var_of_abstract_clause "|") formatter) set
  ;;
  let show = pp_to_string pp;;
  let to_yojson (S(set)) =
    let set_list = Abs_clause_set.elements set in
    `List (List.map abstract_clause_to_yojson set_list)
  ;;
  let name_prefix = "s";;
end;;
