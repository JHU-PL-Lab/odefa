open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_symbolic_interpreter;;
open Interpreter_types;;

open Ast;;

let lazy_logger = Logger_utils.make_lazy_logger "Generator_utils";;

exception Halt_interpretation_as_input_sequence_is_complete;;

(* TODO: Check for correctness *)
(*
let absolutize_stack
    (reference_point : Ident.t list)
    (relstack : Relative_stack.t)
  : Ident.t list =
  let costack, stack = Relative_stack.to_lists relstack in
  (* Start by throwing away everything they have in common. *)
  let rec discard_common start finish =
    match start, finish with
    | x :: start', y :: finish' when equal_ident x y ->
      discard_common start' finish'
    | _ -> start, finish
  in
  let _, finish = discard_common reference_point (List.rev costack) in
  (* Attach the stack to C *)
  stack @ finish
;;
*)

(** Computes a relative stack by calculating the difference between two
    (absolute) stacks.  Given a reference point at which to start, this
    function computes the relative stack which will produce the goal. *)
let relativize_stack
    (reference_point : Ident.t list)
    (goal : Ident.t list)
  : Relative_stack.t =
  let insist f x s =
    match f x s with
    | None -> raise @@ Utils.Invariant_failure "insist got None stack"
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

let destructure_var v =
  match v with
  | Var (_, None) ->
    raise @@ Utils.Invariant_failure
      ("Non-freshened variable " ^ (Ast_pp.show_var v))
  | Var (x, Some(Freshening_stack(stack))) -> (x, stack)
;;

let input_sequence_from_result
    (e : expr)
    (x : Ident.t)
    (result : Interpreter.evaluation_result)
  : (int list * symbol list) =
  match Solver.solve result.er_solver with
  | None ->
    raise @@ Jhupllib_utils.Invariant_failure
      "Attempted to extract input sequence on result with no solution!"
  | Some solution ->
    let (get_value, _) = solution in
    let Concrete_stack stack = result.er_stack in
    let stop_var = Var(x, Some(Freshening_stack(stack))) in
    let (_, stop_stack) = destructure_var stop_var in
    let input_record = ref [] in
    (* Function to call to read from input *)
    let read_from_solver v =
      let (x, stack) = destructure_var v in
      let relstack = relativize_stack stop_stack stack in
      let symbol = Symbol(x, relstack) in
      let value =
        match get_value symbol with
        | None ->
          (* The solver had no value for us.  That means that this variable is
            unconstrained and we are free to pick as we please. *)
          Value_int 0
        | Some value ->
          value
      in
      lazy_logger `trace
        (fun () -> "Reconstructed input: " ^ (Ast_pp.show_value value));
      input_record := value :: !input_record;
      value
    in
    (* Callback function executed on each clause encountered *)
    let stop_at_stop_var (Clause(x, _)) =
      if equal_var x stop_var then
        raise Halt_interpretation_as_input_sequence_is_complete;
    in
    let abort_list = ref [] in
    let accum_abort (Clause(x, _)) : unit =
      lazy_logger `trace (fun () -> Printf.sprintf "%s" (Ast_pp.show_var x));
      abort_list := x :: !abort_list;
    in
    (* Run the interpreter with the above input source and clause callback *)
    let execute_interpreter () =
      try
        let _ =
          Odefa_interpreter.Interpreter.eval
            ~input_source:read_from_solver
            ~clause_callback:stop_at_stop_var
            ~abort_policy:accum_abort
            e
        in
        raise @@ Jhupllib.Utils.Invariant_failure
          "evaluation completed without triggering halt exception!"
      with
      | Halt_interpretation_as_input_sequence_is_complete -> ()
    in
    let () = execute_interpreter () in
    let input_sequence = List.rev !input_record in
    let input_seq_ints =
      List.map
        (fun value ->
          match value with
          | Value_int n -> n
          | _ ->
            raise @@ Jhupllib.Utils.Not_yet_implemented
              "Cannot presently handle non-integer input!"
        )
        input_sequence
    in
    let abort_symbol_list =
      List.map
        (fun ab_var ->
          let (ab_x, ab_stack) = destructure_var ab_var in
          let ab_relstack = relativize_stack stop_stack ab_stack in
          let ab_symb = Symbol (ab_x, ab_relstack) in
          if Symbol_map.mem ab_symb result.er_abort_points then
            ab_symb
          else
            raise @@ Utils.Invariant_failure
              (Printf.sprintf "Encountered unknown abort clause %s"
                (Ast_pp.show_var ab_var))
        )
        !abort_list
    in
    (input_seq_ints, abort_symbol_list)
;;