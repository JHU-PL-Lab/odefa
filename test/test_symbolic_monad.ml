(** This module contains tests for the solver interface in the symbolic
    interpreter. *)

open Batteries;;
open Jhupllib;;
open OUnit2;;

open Odefa_symbolic_interpreter;;

module F = Formula_test_utils;;

(* **** Scaffolding ***** *)

let _tests_acc = ref [];;
let _add_test name testfn = _tests_acc := (name >:: testfn) :: !_tests_acc;;

(* Experimental monad *)

module Cache_key = struct
  type t = int;;
  let compare = Int.compare;;
end;;

module M = Symbolic_monad.Make(struct
    module Cache_key = Cache_key;;
  end);;

open M;;

(* **** Utils **** *)

let complete_with_count (x : 'a m) : ('a * Formulae.t * int) Enum.t =
  let rec loop evaluation steps_so_far
    : ('a * Formulae.t * int) Enum.t =
    let results, evaluation' = step evaluation in
    let new_steps_so_far = steps_so_far + 1 in
    let tagged_results =
      results
      |> Enum.map (fun (value,formulae) -> (value,formulae,new_steps_so_far))
    in
    Enum.append tagged_results @@
    if is_complete evaluation' then Enum.empty () else
      loop evaluation' new_steps_so_far
  in
  loop (start x) 0
;;

let complete (x : 'a m) : ('a * Formulae.t) Enum.t =
  Enum.map (fun (x,y,_) -> (x,y)) @@ complete_with_count x
;;

let test_complete_values
    (type a)
    (printer : a Pp_utils.pretty_printer)
    (equal : a -> a -> bool)
    (computation : a m)
    (expected : a list) : unit =
  let results : (a * Formulae.t * int) Enum.t =
    complete_with_count computation
  in
  let actual : a list =
    List.of_enum @@ Enum.map (fun (x,_,_) -> x) results
  in
  assert_equal
    ~printer:(Pp_utils.pp_to_string @@ Pp_utils.pp_list printer)
    ~cmp:(List.eq equal)
    expected
    actual
;;

let test_complete_values_with_steps
    (type a)
    (printer : a Pp_utils.pretty_printer)
    (equal : a -> a -> bool)
    (computation : a m)
    (expected : (a * int) list) : unit =
  let results : (a * Formulae.t * int) Enum.t =
    complete_with_count computation
  in
  let actual : (a * int) list =
    List.of_enum @@ Enum.map (fun (x,_,y) -> (x,y)) results
  in
  assert_equal
    ~printer:(Pp_utils.pp_to_string @@
              Pp_utils.pp_list @@ Pp_utils.pp_tuple printer Format.pp_print_int)
    ~cmp:(List.eq (fun (x,y) (x',y') -> equal x x' && y = y'))
    expected
    actual
;;

(* **** Tests **** *)

_add_test "pure 4" @@ fun _ ->
let computation =
  return 4
in
test_complete_values_with_steps Format.pp_print_int (=) computation
  [(4,1)]
;;

_add_test "nondeterminism" @@ fun _ ->
let computation =
  let%bind z = pick @@ List.enum [1;2;3;4] in
  return z
in
test_complete_values_with_steps Format.pp_print_int (=) computation
  [(1,1); (2,1); (3,1); (4,1)]
;;

_add_test "nondeterminism and state" @@ fun _ ->
let computation =
  check_formulae @@
  let%bind () = record_formula @@ F.set_int F.x 2 in
  let%bind () = record_formula @@ F.alias F.x F.y in
  let%bind n = pick @@ List.enum [1;2;3] in
  let%bind () = record_formula @@ F.set_int F.y n in
  return n
in
test_complete_values_with_steps Format.pp_print_int (=) computation
  [(2,1)]
;;

_add_test "single pause" @@ fun _ ->
let computation =
  let%bind () = pause () in
  return 5
in
test_complete_values_with_steps Format.pp_print_int (=) computation
  [(5,2)]
;;

_add_test "pause in nondeterminism" @@ fun _ ->
let computation =
  let%bind n = pick @@ List.enum [1;2;3] in
  let%bind () = pause () in
  return n
in
test_complete_values_with_steps Format.pp_print_int (=) computation
  [(1,2); (2,3); (3,4)]
;;

_add_test "nondeterministic pause" @@ fun _ ->
let computation =
  let%bind n = pick @@ List.enum [1;2;3;4;5] in
  let%bind () = if n mod 2 = 0 then pause () else return () in
  return n
in
test_complete_values_with_steps Format.pp_print_int (=) computation
  [(1,1); (3,1); (5,1); (2,2); (4,3)]
;;

(* **** Packaging up tests for main test module ***** *)

let tests = "symbolic monad tests" >::: List.rev !_tests_acc;;
