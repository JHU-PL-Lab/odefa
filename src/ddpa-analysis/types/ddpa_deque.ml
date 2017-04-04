(** This module provides a functor wrapper for Batteries.Deque that provides
    some basic operations such as comparison. *)

open Jhupllib;;

open Pds_reachability_utils;;

module Make(T : Decorated_type) =
struct
  type t = T.t Batteries.Deque.t;;
  let compare x y =
    Batteries.Enum.compare T.compare
      (Batteries.Deque.enum x) (Batteries.Deque.enum y)
  ;;
  let equal x y =
    Batteries.Enum.equal T.equal
      (Batteries.Deque.enum x) (Batteries.Deque.enum y)
  ;;
  let pp formatter x =
    Pp_utils.pp_concat_sep_delim "[" "]" ", " T.pp formatter @@
    Batteries.Deque.enum x
  ;;
  let show x = Pp_utils.pp_to_string pp x;;
  let to_yojson x =
    `List (Batteries.List.of_enum @@ Batteries.Enum.map T.to_yojson @@
           Batteries.Deque.enum x)
  ;;

  let empty = Batteries.Deque.empty;;
  let enum = Batteries.Deque.enum;;
  let front = Batteries.Deque.front;;
  let rear = Batteries.Deque.rear;;
  let singleton x = Batteries.Deque.cons x Batteries.Deque.empty;;
  let size = Batteries.Deque.size;;
  let snoc = Batteries.Deque.snoc;;
end;;
