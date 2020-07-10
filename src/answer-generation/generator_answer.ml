open Batteries;;
(* open Jhupllib;; *)

open Odefa_ast;;
open Ast;;
open Ast_pp;;


open Odefa_symbolic_interpreter;;
open Odefa_symbolic_interpreter.Interpreter_types;;
open Odefa_symbolic_interpreter.Interpreter;;
open Odefa_symbolic_interpreter.Solver;;
(* open Odefa_symbolic_interpreter.Relative_stack;; *)

(* let lazy_logger = Logger_utils.make_lazy_logger "Generator_answer";; *)

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

  let answer_from_result e x result =
    let (input_seq, _) =
      Generator_utils.input_sequence_from_result e x result
    in
    input_seq
  ;;

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

  type type_error = {
    terr_expected_type : type_sig;
    terr_actual_type : type_sig;
    terr_operation : clause;
    terr_var_definition : (ident * value)
  }

  type error_seq = {
    err_type_errors : type_error list;
    err_input_seq : int list
  }

  type t = error_seq

  let _symbol_to_var_value
      (stop_stack : Ident.t list)
      (symb_value : Constraint.value)
    : Ast.value =
    match symb_value with
    | Constraint.Int n -> Ast.Value_int n
    | Constraint.Bool b -> Ast.Value_bool b
    | Constraint.Function f -> Ast.Value_function f
    | Constraint.Record symb_map ->
      begin
        let var_map =
          Ident_map.map
            (fun (Symbol(ident, relstack)) ->
              let abstack = Generator_utils.absolutize_stack stop_stack relstack in
              Var (ident, Some (Ast.Freshening_stack (abstack)))
            )
            symb_map
        in
        Ast.Value_record (Record_value var_map)
      end
    ;;

  let answer_from_result e x result =
    let Concrete_stack stop_stack = result.er_stack in
    let solver = result.er_solver in
    let (input_seq, _) =
      Generator_utils.input_sequence_from_result e x result
    in
    let abort_points = result.er_abort_points in
    let type_err_lst =
      abort_points
      |> Symbol_map.enum
      |> Enum.fold
          (fun accum (ab_symb, ab_info) ->
            let Symbol(_, relstack) = ab_symb in
            match ab_info with
            | Type_abort_info type_ab_info ->
              let match_imap = type_ab_info.abort_matches in
              let err_list =
                match_imap
                |> Ident_map.enum
                |> Enum.filter_map
                  (fun (ident, _) ->
                    let type_err =
                      find_type_error solver (Symbol(ident, relstack))
                    in
                    match type_err with
                    | None -> None
                    | Some type_err_rec ->
                      let terr_value =
                        _symbol_to_var_value stop_stack type_err_rec.terr_value
                      in
                      Some {
                        terr_expected_type = type_err_rec.terr_expected_type;
                        terr_actual_type = type_err_rec.terr_actual_type;
                        terr_operation = type_ab_info.abort_operation;
                        terr_var_definition = (type_err_rec.terr_ident, terr_value);
                      }
                  )
                |> List.of_enum
              in
              err_list @ accum
            | Match_abort_info _ -> accum (* Not implemented yet! *)
          )
          []
    in
    { err_type_errors = type_err_lst;
      err_input_seq = input_seq;
    }
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
    { err_type_errors = [];
      err_input_seq = []
    }
  ;;

  (* TODO: Show a "no type errors" message if type_error_seq is empty *)
  let show error_seq =
    let show_type_error type_error =
      let (ident, value) = type_error.terr_var_definition in
      "* Operation:  " ^ (show_clause type_error.terr_operation) ^ "\n" ^
      "* Definition: " ^ (show_ident ident) ^" = "^ (show_value value) ^ "\n" ^
      "* Expected:   " ^ (show_type_sig type_error.terr_expected_type) ^ "\n" ^
      "* Actual:     " ^ (show_type_sig type_error.terr_actual_type) ^ "\n"
    in
    let show_input_seq inputs =
      "[" ^ (String.join ", " @@ List.map string_of_int inputs) ^ "]"
    in
    ("Type errors for input sequence " ^
    (show_input_seq error_seq.err_input_seq) ^ "\n" ^
    (String.join "\n" (List.map show_type_error error_seq.err_type_errors)))
  ;;

  let empty = {
    err_type_errors = [];
    err_input_seq = [];
  };;

  let is_empty type_errors = List.is_empty type_errors.err_type_errors;;

  let count type_errors = List.length type_errors.err_type_errors;;
end;;