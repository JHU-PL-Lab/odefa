(** A "context stack" which performs no actions. *)

open Batteries;;

open Analysis_context_stack;;

module Stack : Context_stack =
struct
  type t = unit;;
  let compare () () = 0;;
  let empty = ();;
  let push _ () = Enum.singleton @@ ();;
  let pop () = Enum.singleton @@ ();;
  let is_top _ () = true;;
  let pp () = "-";;
  let ppa = pp;;
  let name = "0ddpa";;
end;;
