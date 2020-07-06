open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Ast;;
open Ast_pp;;

open Odefa_symbolic_interpreter.Interpreter_types;;
open Odefa_symbolic_interpreter.Interpreter;;
open Odefa_symbolic_interpreter.Solver;;
open Odefa_symbolic_interpreter.Relative_stack;;

let lazy_logger = Logger_utils.make_lazy_logger "Generator_answer";;

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

(* **** Input sequence **** *)

module Input_sequence : Answer = struct
  type t = int list;;

  exception Halt_interpretation_as_input_sequence_is_complete;;

  let input_sequence_from_solution
      (solution : solution)
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
      let relstack = Generator_utils.relativize_stack stop_stack stack in
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
        (* Fail silently *)
        lazy_logger `trace (fun () ->
          Printf.sprintf
            "Execution failed at abort clause %s"
            (show_var ab_var));
        ()
        (* raise (Jhupllib.Utils.Invariant_failure ("abort failure at " ^ (show_var ab_var))) *)
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
    match solve solver with
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

(* **** Type Errors **** *)

module Type_errors : Answer = struct
  (* type t = (ident * type_sig * type_sig) list *)

  type type_error = {
    odefa_var : Ident.t;
    (* define_clause : clause; *)
    (* use_clause : clause; *)
    expected_type : type_sig;
    actual_type : type_sig;
    (* input_seq : int list option; *)
  }

  type t = type_error list

  let answer_from_result (_: expr) (_: ident) result =
    List.map (fun type_err ->
      let (var, expected, actual) = type_err in
      { odefa_var = var;
        expected_type = expected;
        actual_type = actual;
      }
    )
    result.er_type_errors
  ;;

  (*
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
  *)

  (* TEMP *)
  let answer_from_string (arg_str : string) =
    let _ = arg_str in
    [{ odefa_var = Ident("foo");
      expected_type = Bool_type;
      actual_type = Int_type }]
  ;;

  let show type_errors =
    let show_one_type_error type_error =
      "* Variable: " ^ (show_ident type_error.odefa_var) ^ "\n" ^
      "* Expected: " ^ (show_type_sig type_error.expected_type) ^ "\n" ^
      "* Actual: " ^ (show_type_sig type_error.actual_type) ^ "\n"
    in
    String.join "\n" @@ List.map show_one_type_error type_errors
  ;;

  let empty = [];;

  let is_empty type_errors = List.is_empty type_errors;;

  let count type_errors = List.length type_errors;;
end;;