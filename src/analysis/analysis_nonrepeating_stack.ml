open Batteries;;

open Analysis_context_stack;;
open Ddpa_graph;;
open String_utils;;

module Stack : Context_stack =
struct
  type t = S of (abstract_clause list * Abs_clause_set.t);;
  let compare = compare;;
  let empty = S([],Abs_clause_set.empty);;
  let push c (S(c_list,c_set)) =
    if Abs_clause_set.mem c c_set
    then Enum.singleton @@
      S( c :: (List.take_while (fun c' -> c <> c') c_list)
       , Abs_clause_set.singleton c)
    else Enum.singleton @@ S(c :: c_list, Abs_clause_set.add c c_set)
  ;;
  let pop (S(c_list,c_set)) =
    match c_list with
    | [] -> Enum.singleton @@ empty
    | h::t -> Enum.singleton @@ S(t, Abs_clause_set.remove h c_set)
  ;;
  let is_top c (S(c_list,_)) =
    match c_list with
    | [] -> true
    | h::_ -> c = h
  ;;
  let pp (S(c_list,_)) =
    concat_sep "|" @@
    Enum.append
      (Enum.map pp_abstract_clause @@ List.enum c_list)
      (Enum.singleton "?")
  ;;
  let ppa = pp;;
  let name = "ddpaNR";;
end;;
