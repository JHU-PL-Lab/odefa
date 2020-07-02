open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ast_pp;;

open Generator_configuration;;

open Odefa_symbolic_interpreter.Interpreter_types;;
open Odefa_symbolic_interpreter.Interpreter;;

module Symbolic_interpreter = Odefa_symbolic_interpreter;;
module Relative_stack = Symbolic_interpreter.Relative_stack;;
module Solver = Symbolic_interpreter.Solver;;

let lazy_logger = Logger_utils.make_lazy_logger "Generator";;

exception Parse_failure;;

module type Answer = sig
  type t;;
  val answer_from_result : expr -> ident -> evaluation_result -> t;;
  val answer_from_string : string -> t;;
  val show : t -> string;;
  val empty : t;;
  val is_empty : t -> bool;;
  val count : t -> int;;
end;;

module type Generator = sig
  module Answer : Answer;;

  type generator;;
  type generation_result;;

  val create :
    ?exploration_policy:exploration_policy ->
    configuration -> expr -> ident -> generator;;
  val generate_answers :
    ?generation_callback:(Answer.t -> int -> unit) ->
    int option -> generator ->
    (Answer.t list * int) list * generator option;;
end;;

module Input_sequence : Answer = struct
  type t = int list;;
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

  exception Halt_interpretation_as_input_sequence_is_complete;;

  let input_sequence_from_solution
      (solution : Solver.solution)
      (e : expr)
      (stop_var : Var.t)
    : int list =
    let (get_value, _) = solution in
    let stop_stack =
      match stop_var with
      | Var(_, None) ->
        raise @@ Jhupllib.Utils.Invariant_failure
          "Non-freshened stop variable!"
      | Var(_, Some(Freshening_stack(stop_stack))) -> stop_stack
    in
    let input_record = ref [] in
    let read_from_solver (Var(x,stack_opt)) =
      let stack =
        match stack_opt with
        | None ->
          raise @@ Jhupllib.Utils.Invariant_failure
            "Interpreter performed input on non-freshened variable!"
        | Some(Freshening_stack(stack)) ->
          stack
      in
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
    let stop_at_stop_var (Clause(x,_)) =
      if equal_var x stop_var then
        raise Halt_interpretation_as_input_sequence_is_complete
      else
        ()
    in
    begin
      try
        let _ =
          Odefa_interpreter.Interpreter.eval
            ~input_source:read_from_solver
            ~clause_callback:stop_at_stop_var
            e
        in
        raise @@ Jhupllib.Utils.Invariant_failure
          "evaluation completed without triggering halt exception!"
      with
      | Halt_interpretation_as_input_sequence_is_complete -> ()
      (* TODO: check that the abort var is in the set of abort clauses encountered during symbolic lookup. 
        Otherwise we have an Invariant_failure*)
      | Odefa_interpreter.Interpreter.Abort_failure ab_var ->
        lazy_logger `trace (fun () ->
          Printf.sprintf
            "Execution failed at abort clause %s"
            (show_var ab_var));
        ()
    end;
    let input_sequence = List.rev !input_record in
    input_sequence
    |> List.map
      (fun value ->
        match value with
        | Value_int n -> n
        | _ ->
          raise @@ Jhupllib.Utils.Not_yet_implemented
            "cannot presently handle non-integer input!"
      )
  ;;

  let input_sequence_from_result e x result =
    let solver = result.er_solver in
    match Solver.solve solver with
    | None ->
      raise @@ Jhupllib_utils.Invariant_failure
        "input_sequence_from_result (no solution)"
    | Some solution ->
      let Concrete_stack stack =
        result.er_stack
      in
      let stop_var = Var(x, Some(Freshening_stack(stack))) in
      let input_sequence =
        input_sequence_from_solution solution e stop_var
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Yielding input sequence: %s"
            (String.join "," @@ List.map string_of_int input_sequence)
        );
      input_sequence
  ;;

  let answer_from_result = input_sequence_from_result;;

  (* String "[ 1, 2, 3 ]" or "1, 2, 3" to input sequence *)
  let answer_from_string arg_str =
    (* let arg_char_lst = String.to_list arg_str in *)
    let arg_str' =
      begin
        if (String.starts_with arg_str "[")
            && (String.ends_with arg_str "]") then
          arg_str
          |> String.lchop
          |> String.rchop
        else
          arg_str
      end
    in
    let str_lst =
      arg_str'
      |> Str.global_replace (Str.regexp "[ ]*") ""
      |> Str.split (Str.regexp ",")
    in
    try
      List.map int_of_string str_lst
    with Failure _ ->
      raise Parse_failure
  ;;

  let show inputs =
    "[" ^ (String.join ", " @@ List.map string_of_int inputs) ^ "]"
  
  let empty = [];;

  let is_empty inputs = List.is_empty inputs;;

  let count inputs = List.length inputs;;
end;;

module Type_errors : Answer = struct
  type t = (ident * type_sig * type_sig) list

  let answer_from_result _ _ result =
    result.er_type_errors
  ;;

  let answer_from_string arg_str =
    let arg_str' =
      begin
        if (String.starts_with arg_str "[")
          && (String.ends_with arg_str "]") then
          arg_str
          |> String.lchop
          |> String.rchop
        else
          arg_str
      end
    in
    let str_lst =
      arg_str'
      |> Str.global_replace (Str.regexp "[ ]*") ""
      |> Str.split (Str.regexp ",")
    in
    List.map (fun str ->
      str
      |> Str.global_replace (Str.regexp "[)(]") ""
      |> (fun str -> 
        let lst = Str.split (Str.regexp "[,]") str in
        match lst with
        | [s1; s2; s3] ->
          let to_type_err s =
            match s with
            | "int" -> Int_type
            | "bool" -> Bool_type
            | "fun" -> Fun_type
            | "rec" -> Rec_type (Ident_set.empty)
            | _ -> raise Parse_failure
          in
          (Ident(s1), to_type_err s2, to_type_err s3)
        | _ -> raise Parse_failure 
      )
    )
    str_lst
  ;;

  let show type_errors =
    let show_one_type_error type_error =
      let (id, typ1, typ2) = type_error in
      "* Variable: " ^ (show_ident id) ^ "\n" ^
      "* Expected: " ^ (show_type_sig typ1) ^ "\n" ^
      "* Actual: " ^ (show_type_sig typ2) ^ "\n"
    in
    String.join ", " @@ List.map show_one_type_error type_errors
  ;;

  let empty = [];;

  let is_empty type_errors = List.is_empty type_errors;;

  let count type_errors = List.length type_errors;;
end;;

module Make(Answer : Answer) : Generator = struct
  module Answer = Answer;;

  type generator =
    {
      gen_program : expr;
      gen_target : Ident.t;
      generator_fn : (int -> generation_result) option
    }

  and generation_result =
    {
      gen_answers : Answer.t list;
      gen_steps : int;
      gen_generator : generator;
    }
  ;;

  let rec take_steps
      (e : expr)
      (x : Ident.t)
      (max_steps : int)
      (evaluation : evaluation)
    : generation_result =
    let rec loop
        (step_count : int)
        (ev : evaluation)
      : generation_result =
      lazy_logger `trace (fun () -> Printf.sprintf
                            "%d/%d completed in this pass" step_count max_steps);
      if step_count = max_steps then begin
        lazy_logger `trace (fun () ->
            "Pass reached max step count; returning suspended generator.");
        { gen_answers = [];
          gen_steps = step_count;
          gen_generator =
            { gen_program = e;
              gen_target = x;
              generator_fn = Some(fun n -> take_steps e x n ev)
            };
        }
      end else begin
        let results, ev'_opt = step ev in
        if List.is_empty results then begin
          lazy_logger `trace (fun () ->
              "No new results found in this step.");
          match ev'_opt with
          | Some ev' ->
            (* No result and no termination.  Keep running. *)
            lazy_logger `trace (fun () ->
                "Interpreter evaluation not yet complete; continuing.");
            loop (step_count + 1) ev'
          | None ->
            (* No result and no remaining computation; we terminated!  Give back a
              result indicating as much. *)
            lazy_logger `trace (fun () ->
                "Interpreter evaluation complete; stopping.");
            { gen_answers = [];
              gen_steps = step_count + 1;
              gen_generator =
                { gen_program = e;
                  gen_target = x;
                  generator_fn = None;
                };
            }
        end else begin
          (* We have results! *)
          lazy_logger `trace (fun () -> "Found input sequences!");
          let answers = List.map (Answer.answer_from_result e x) results in
          let generator_fn =
            match ev'_opt with
            | None -> None
            | Some ev' -> Some(fun n -> take_steps e x n ev')
          in
          { gen_answers = answers;
            gen_steps = step_count + 1;
            gen_generator =
              { gen_program = e;
                gen_target = x;
                generator_fn = generator_fn;
              };
          }
        end
      end
    in
    loop 0 evaluation
  ;;

  let create
      ?exploration_policy:(exploration_policy=Explore_breadth_first)
      (conf : configuration)
      (e : expr)
      (x : ident)
    : generator =
    let module Stack = (val conf.conf_context_model) in
    let module Analysis = Ddpa_analysis.Make(Stack) in
    let cfg =
      e
      |> Analysis.create_initial_analysis
      |> Analysis.perform_full_closure
      |> Analysis.cfg_of_analysis
    in
    let evaluation =
      start
      ~exploration_policy:exploration_policy
      cfg e x
    in
    { gen_program = e;
      gen_target = x;
      generator_fn = Some(fun n -> take_steps e x n evaluation)
    }
  ;;

  let generate_answers
    ?generation_callback:(generation_callback=fun _ _ -> ())
    (max_steps_opt : int option)
    (original_generator : generator)
  : (Answer.t list * int) list * generator option =
  lazy_logger `trace
    (fun () -> "Generating inputs for expression:\n" ^
               Pp_utils.pp_to_string pp_expr original_generator.gen_program
    );
  let max_steps_per_loop = 100 in
  let rec loop
      (gen : generator)
      (steps_left_opt : int option)
      (steps_taken : int)
      (results : (Answer.t list * int) list)
    : (Answer.t list * int) list * generator option =
    let steps_to_take =
      match steps_left_opt with
      | None -> max_steps_per_loop
      | Some n -> min n max_steps_per_loop
    in
    if steps_to_take = 0 then begin
      (* We're quitting now! *)
      lazy_logger `trace
        (fun () -> "Out of generation steps; stopping with waiting generator.");
      (results, Some gen)
    end else begin
      lazy_logger `trace
        (fun () -> Printf.sprintf
            "Taking up to %d step%s of generation in this loop" steps_to_take
            (if steps_to_take = 1 then "" else "s"));
      match gen.generator_fn with
      | None ->
        (* No further generation is possible. *)
        lazy_logger `trace
          (fun () -> "Generation terminated with no further results.");
        (results, None)
      | Some fn ->
        let result = fn steps_to_take in
        let steps_taken' = steps_taken + result.gen_steps in
        lazy_logger `trace
          (fun () -> Printf.sprintf "Took %d step%s (%d so far)"
              result.gen_steps
              (if result.gen_steps = 1 then "" else "s")
              steps_taken');
        begin
          match result.gen_answers with
          | _ :: _ ->
            List.iter
              (fun answer ->
                lazy_logger `trace
                  (fun () -> "Discovered an answer on this iteration.");
                generation_callback answer steps_taken')
              result.gen_answers
          | [] ->
            lazy_logger `trace
              (fun () ->
                 "No further answer discovered in this iteration.");
        end;
        let results' = [(result.gen_answers, steps_taken')] @ results
        in
        let steps_left_opt' =
          Option.map (fun n -> max 0 @@ n - result.gen_steps) steps_left_opt
        in
        loop result.gen_generator steps_left_opt' steps_taken' results'
    end
  in
  loop original_generator max_steps_opt 0 []
;;
end;;

(*
let type_errors_from_result result =
  let type_errors = result.Symbolic_interpreter.Interpreter.er_type_errors in
  type_errors
;;
*)