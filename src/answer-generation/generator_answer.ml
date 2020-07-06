open Batteries;;
(* open Jhupllib;; *)

open Odefa_ast;;
open Ast;;
open Ast_pp;;

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
    let solver = result.er_solver in
    let find_type_error_fn = find_type_error solver in
    result.er_abort_points
    |> Symbol_map.enum
    |> Enum.fold (fun accum (_, var_list) ->
        let err_list =
          List.filter_map
            (fun v ->
              let type_err = find_type_error_fn v in
              match type_err with
              | None -> None
              | Some (v', expected, actual) ->
                Some {
                      odefa_var = v';
                      expected_type = expected;
                      actual_type = actual;
                    }
            )
            var_list
        in
        err_list @ accum
      ) []
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