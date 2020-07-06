open Batteries;;
(* open Jhupllib;; *)

open Odefa_ast;;
open Odefa_symbolic_interpreter;;

open Ast;;

(** Calculates the difference between two stacks.  Given a reference point at
    which to start, this function computes the relative stack which will produce
    the goal. *)
let relativize_stack
    (reference_point : Ident.t list)
    (goal : Ident.t list)
    : Relative_stack.t =
  let insist f x s =
    match f x s with
    | None -> raise @@ Jhupllib.Utils.Invariant_failure "insist got None stack"
    | Some s' -> s'
  in
  (* Start by throwing away everything they have in common. *)
  let rec discard_common start finish =
    match start, finish with
    | x :: start', y :: finish' when equal_ident x y ->
      discard_common start' finish'
    | _ -> start, finish
  in
  let start, finish = discard_common reference_point goal in
  (* To get from the start stack to the finish stack, we'll first have to pop
    everything from the start stack and then push everything from the finish
    stack. *)
  let relstack =
    Relative_stack.empty
    |> List.fold_right (flip @@ insist Relative_stack.pop) (List.rev start)
    |> (flip @@ List.fold_left (insist Relative_stack.push)) (List.rev finish)
  in
  relstack
;;

