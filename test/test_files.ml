(**
   This test module will load a series of test files from the test sources
   directory and execute them one by one.

   Each file is expected to contain a comment describing the expected test
   result.  The comment should be of one of the following forms:

    - [EXPECT-EVALUATE] (which requires that the code evaluates to completion)
    - [EXPECT-STUCK] (which requires that the code gets stuck)
   FIXME: update this documentation
*)

(* FIXME: purge the term "inconsistency" *)

open Batteries;;
open Jhupllib;;
open OUnit2;;

open Core_ast;;
open Core_ast_pp;;
open Core_ast_wellformedness;;
open Core_toploop_options;;
open Core_toploop_types;;
open Ddpa_abstract_ast;;
open Ddpa_abstract_stores;;
open String_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Test_files";;

exception File_test_creation_failure of string;;

type test_expectation =
  | Expect_evaluate
  | Expect_stuck
  | Expect_well_formed
  | Expect_ill_formed
  | Expected_maximum_stack_delta_size_is of int option
  | Expect_analysis_variable_lookup_from_end of ident * string
  | Expect_analysis_inconsistency_at of ident
  | Expect_analysis_no_inconsistencies
;;

let pp_test_expectation formatter expectation =
  match expectation with
  | Expect_evaluate -> Format.pp_print_string formatter "Expect_evaluate"
  | Expect_stuck -> Format.pp_print_string formatter "Expect_stuck"
  | Expect_well_formed -> Format.pp_print_string formatter "Expect_well_formed"
  | Expect_ill_formed -> Format.pp_print_string formatter "Expect_ill_formed"
  | Expected_maximum_stack_delta_size_is n ->
    Format.fprintf formatter "Expect_analysis_stack_is(%s)"
      (n |> Option.map string_of_int |> Option.default "none")
  | Expect_analysis_variable_lookup_from_end(x,expected) ->
    Format.fprintf formatter
      "Expect_analysis_variable_lookup_from_end(%a,\"%s\")"
      pp_ident x expected
  | Expect_analysis_inconsistency_at(x) ->
    Format.fprintf formatter
      "Expect_analysis_inconsistency_at(%a)"
      pp_ident x
  | Expect_analysis_no_inconsistencies ->
    Format.pp_print_string formatter "Expect_analysis_no_inconsistencies"
;;

type expectation_parse =
  | Parse_success of test_expectation
  | Parse_failure of string
;;

exception Expectation_parse_failure of string;;

exception Expectation_not_found;;

type expectation_stack_decision =
  | Default_size
  | Chosen_size of int option
;;

let parse_expectation str =
  let assert_no_args lst =
    if List.is_empty lst
    then ()
    else raise @@ Expectation_parse_failure "expected no arguments"
  in
  let assert_one_arg lst =
    match lst with
    | [x] -> x
    | _ ->
      raise @@
      Expectation_parse_failure ("expected one argument; got " ^
                                 string_of_int (List.length lst))
  in
  let assert_two_args lst =
    match lst with
    | [x;y] -> (x,y)
    | _ ->
      raise @@
      Expectation_parse_failure ("expected two arguments; got " ^
                                 string_of_int (List.length lst))
  in
  try
    let expectation =
      match String_utils.whitespace_split ~max:2 str with
      | "EXPECT-EVALUATE"::args_part ->
        assert_no_args args_part;
        Expect_evaluate
      | "EXPECT-STUCK"::args_part ->
        assert_no_args args_part;
        Expect_stuck
      | "EXPECT-WELL-FORMED"::args_part ->
        assert_no_args args_part;
        Expect_well_formed
      | "EXPECT-ILL-FORMED"::args_part ->
        assert_no_args args_part;
        Expect_ill_formed
      | "EXPECT-MAX-STACK-DELTA-SIZE"::args_part ->
        let args_str = String.join "" args_part in
        let args = whitespace_split args_str in
        let max_stack_delta_size_str = assert_one_arg args in
        let max_stack_delta_size_choice =
          if max_stack_delta_size_str = "none"
          then None
          else
            try
              let max_stack_delta_size =
                int_of_string max_stack_delta_size_str
              in
              if max_stack_delta_size >= 0
              then Some max_stack_delta_size
              else
                raise @@ Expectation_parse_failure "negative stack delta size"
            with
            | Failure _ ->
              raise @@ Expectation_parse_failure "invalid stack delta size"
        in
        Expected_maximum_stack_delta_size_is max_stack_delta_size_choice
      | "EXPECT-ANALYSIS-LOOKUP-FROM-END"::args_part ->
        let args_str = String.join "" args_part in
        let args = whitespace_split ~max:2 args_str in
        let (ident_str, pp_expectation) = assert_two_args args in
        let ident = Ident(ident_str) in
        Expect_analysis_variable_lookup_from_end(ident,pp_expectation)
      | "EXPECT-ANALYSIS-INCONSISTENCY-AT"::args_part ->
        let args_str = String.join "" args_part in
        let args = whitespace_split args_str in
        let call_site = assert_one_arg args in
        Expect_analysis_inconsistency_at (Ident(call_site))
      | "EXPECT-ANALYSIS-NO-INCONSISTENCIES"::args_part ->
        assert_no_args args_part;
        Expect_analysis_no_inconsistencies
      | expect_str::_
        when
          String.length expect_str >= 7 &&
          String.equal "EXPECT-" (String.sub expect_str 0 7) ->
        raise @@ Expectation_parse_failure("Unknown expectation: " ^ expect_str)
      | _ ->
        raise @@ Expectation_not_found
    in
    Some (Parse_success expectation)
  with
  | Expectation_parse_failure s -> Some (Parse_failure s)
  | Expectation_not_found -> None
;;

let observe_evaluated expectation =
  match expectation with
  | Expect_evaluate -> None
  | Expect_stuck ->
    assert_failure @@ "Evaluation completed but was expected to become stuck."
  | _ -> Some expectation
;;

let observe_stuck failure expectation =
  match expectation with
  | Expect_evaluate ->
    assert_failure @@ "Evaluation became stuck: " ^ failure
  | Expect_stuck -> None
  | _ -> Some expectation
;;

let observe_evaluation_invalidated expectation =
  match expectation with
  | Expect_evaluate
  | Expect_stuck ->
    assert_failure @@ "Evaluation cancelled due to inconsistency"
  | _ -> Some expectation
;;

let observe_well_formed expectation =
  match expectation with
  | Expect_well_formed -> None
  | Expect_ill_formed ->
    assert_failure @@ "Well-formedness check passed but was expect to fail."
  | _ -> Some expectation
;;

let observe_ill_formed illformednesses expectation =
  match expectation with
  | Expect_well_formed ->
    assert_failure @@ "Expression was unexpectedly ill-formed.  Causes:" ^
                      "\n    * " ^ concat_sep "\n    *"
                        (List.enum @@
                         List.map show_illformedness illformednesses)
  | Expect_ill_formed -> None
  | _ -> Some expectation
;;

let observe_analysis_stack_selection chosen_stack_ref expectation =
  match expectation with
  | Expected_maximum_stack_delta_size_is size_option ->
    begin
      chosen_stack_ref :=
        match !chosen_stack_ref with
        | Default_size -> Chosen_size size_option
        | Chosen_size _ ->
          assert_failure @@ "multiple expectations of analysis stack"
    end;
    None
  | _ -> Some expectation
;;

let observe_analysis_variable_lookup_from_end ident repr expectation =
  match expectation with
  | Expect_analysis_variable_lookup_from_end(ident',repr') ->
    if ident = ident'
    then
      begin
        if repr = repr'
        then None
        else assert_failure @@
          Printf.sprintf "for variable %s, expected %s but got %s"
            (show_ident ident) repr' repr
      end
    else Some expectation
  | _ -> Some expectation
;;

let observe_inconsistency inconsistency expectation =
  let site_of_inconsistency =
    let open Core_toploop_analysis_types in
    match inconsistency with
    | Application_of_non_function (Abs_var ident,_,_,_) -> ident
    | Projection_of_non_record (Abs_var ident,_,_) -> ident
    | Projection_of_absent_label (Abs_var ident,_,_,_) -> ident
    | Deref_of_non_ref (Abs_var ident,_,_) -> ident
    | Update_of_non_ref (Abs_var ident,_,_) -> ident
    | Invalid_binary_operation (Abs_var ident,_,_,_,_,_) -> ident
    | Invalid_unary_operation (Abs_var ident,_,_,_) -> ident
    | Invalid_indexing_subject (Abs_var ident,_,_) -> ident
    | Invalid_indexing_argument (Abs_var ident,_,_) -> ident
  in
  match expectation with
  | Expect_analysis_inconsistency_at expected_site ->
    if site_of_inconsistency = expected_site
    then None
    else Some expectation
  | _ -> Some expectation
;;

let observe_no_inconsistency expectation =
  match expectation with
  | Expect_analysis_no_inconsistencies -> None
  | _ -> Some expectation
;;

let make_test filename expectations =
  let name_of_expectation expectation = match expectation with
    | Expect_evaluate -> "should evaluate"
    | Expect_stuck -> "should get stuck"
    | Expect_well_formed -> "should be well-formed"
    | Expect_ill_formed -> "should be ill-formed"
    | Expected_maximum_stack_delta_size_is size_option ->
      let name =
        match size_option with
        | Some size -> string_of_int size
        | None -> "none"
      in
      "should use analysis stack " ^ name
    | Expect_analysis_variable_lookup_from_end(ident,_) ->
      "should have particular values for variable " ^ (show_ident ident)
    | Expect_analysis_inconsistency_at ident ->
      "should be inconsistent at " ^ show_ident ident
    | Expect_analysis_no_inconsistencies ->
      "should be consistent"
  in
  let test_name = filename in
  (* Create the test in a thunk. *)
  test_name >::
  function _ ->
    lazy_logger `trace (fun () ->
        Printf.sprintf "Performing test for %s with expectations: %s"
          filename
          (Pp_utils.pp_to_string (Pp_utils.pp_list pp_test_expectation)
             expectations)
      );
    (* Using a mutable list of not-yet-handled expectations. *)
    let expectations_left = ref expectations in
    (* This routine takes an observation function and applies it to all of the
       not-yet-handled expectations. *)
    let observation f =
      expectations_left := List.filter_map f @@ !expectations_left;
      lazy_logger `trace (fun () ->
          Printf.sprintf "In test for %s, expectations remaining after an observation: %s"
            filename
            (Pp_utils.pp_to_string (Pp_utils.pp_list pp_test_expectation)
               !expectations_left)
        );
    in
    (* This routine detects expectations of a particular form. *)
    let have_expectation pred = List.exists pred (!expectations_left) in
    (* We're going to execute the following block.  If it completes without
       error, we're also going to require that all of its expectations were
       satisfied.  This addresses nonsense cases such as expecting an ill-formed
       expression to evaluate. *)
    begin
      let expr = File.with_file_in filename Core_parser.parse_program in
      (* Decide what kind of analysis to perform. *)
      let size_choice = ref Default_size in
      observation (observe_analysis_stack_selection size_choice);
      let chosen_size_option =
        match !size_choice with
        | Default_size -> Some 1
        | Chosen_size value -> value
      in
      (* Configure the toploop *)
      let variables_to_analyze =
        !expectations_left
        |> List.enum
        |> Enum.filter_map
          (function
            | Expect_analysis_variable_lookup_from_end(ident,expected) ->
              Some(ident,expected)
            | _ -> None)
        |> List.of_enum
      in
      let configuration =
        { topconf_max_stack_delta = chosen_size_option
        ; topconf_log_prefix = filename ^ "_"
        ; topconf_ddpa_log_level = None
        ; topconf_pdr_log_level = None
        ; topconf_pdr_log_deltas = false
        ; topconf_graph_log_file_name = "ddpa_test.log.yojson"
        ; topconf_analyze_vars =
            if variables_to_analyze = []
            then Core_toploop_option_parsers.Analyze_no_variables
            else
              Core_toploop_option_parsers.Analyze_specific_variables
                (variables_to_analyze
                 |> List.map (fun (Ident s, _) -> (s, None)))
        ; topconf_disable_evaluation =
            not @@ have_expectation
              (function
                | Expect_evaluate -> true
                | Expect_stuck -> true
                | _ -> false)
        ; topconf_disable_inconsistency_check =
            not @@ have_expectation
              (function
                | Expect_analysis_no_inconsistencies -> true
                | Expect_analysis_inconsistency_at _ -> true
                | _ -> false)
        ; topconf_disable_analysis = false
        ; topconf_report_sizes = false
        ; topconf_wddpac_interpreter = false
        ; topconf_forward_interpreter = false
        ; topconf_python_compiler = false
        ; topconf_call_by_need = false
        ; topconf_wddpac_interpreter_map = false
        }
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Test for %s: executing toploop handler"
            filename
        );
      (* Run the toploop *)
      let result =
        Core_toploop.handle_expression
          configuration
          expr
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Test for %s: toploop result was %s"
            filename (Pp_utils.pp_to_string pp_result result)
        );
      (* Report well-formedness or ill-formedness as appropriate. *)
      if result.illformednesses = []
      then observation @@ observe_well_formed
      else observation @@ observe_ill_formed result.illformednesses;
      (* Report each discovered error *)
      result.errors
      |> List.iter
        (fun error -> observation @@ observe_inconsistency error);
      (* If there are no errors, report that. *)
      if result.errors = [] then observation observe_no_inconsistency;
      (* Report each resulting variable analysis. *)
      result.analyses
      |> List.iter
        (fun ((varname,_),stores) ->
           let values =
             stores
             |> Abstract_store_set.enum
             |> Enum.map store_read
             |> Abs_value_set.of_enum
           in
           let repr =
             Pp_utils.pp_to_string
               Abs_value_set.pp values
           in
           observation @@ observe_analysis_variable_lookup_from_end
             (Ident varname) repr
        );
      (* Now report the result of evaluation. *)
      begin
        match result.evaluation_result with
        | Evaluation_completed _ -> observation observe_evaluated
        | Evaluation_failure failure -> observation (observe_stuck failure)
        | Evaluation_invalidated -> observation observe_evaluation_invalidated
        | Evaluation_disabled -> ()
      end;
      (* If there are any expectations of errors left, they're a problem. *)
      !expectations_left
      |> List.iter
        (function
          | Expect_analysis_inconsistency_at ident ->
            assert_failure @@ "Expected error at " ^
                              show_ident ident ^ " which did not occur"
          | _ -> ()
        );
    end;
    (* Now assert that every expectation has been addressed. *)
    match !expectations_left with
    | [] -> ()
    | expectations' ->
      assert_failure @@ "The following expectations could not be met:" ^
                        "\n    * " ^ concat_sep "\n    * "
                          (List.enum @@
                           List.map name_of_expectation expectations')
;;

let make_test_from filename =
  let expectations =
    filename
    |> File.lines_of
    |> Enum.filter_map
      (fun str ->
         let str' = String.trim str in
         if String.starts_with str' "#"
         then
           let str'' = String.trim @@ String.tail str' 1 in
           match parse_expectation str'' with
           | Some (Parse_success expectation) -> Some(Parse_success expectation)
           | Some (Parse_failure s) -> Some(Parse_failure(
               Printf.sprintf
                 "Error parsing expectation:\n        Error: %s\n        Text:  %s"
                 s str''))
           | None -> None
         else None
      )
    |> List.of_enum
  in
  let failures =
    expectations
    |> List.filter_map
      (function
        | Parse_success _ -> None
        | Parse_failure s -> Some s
      )
  in
  match failures with
  | [] ->
    let successes =
      expectations
      |> List.filter_map
        (function
          | Parse_success expectation -> Some expectation
          | Parse_failure _ -> None
        )
    in
    begin
      match successes with
      | [] -> raise (File_test_creation_failure(
          "Could not create test from file " ^ filename ^
          ": no expectations found"))
      | _ ->
        make_test filename successes
    end
  | _ ->
    let message = "Could not create test from file " ^ filename ^ ":" in
    let message' =
      failures
      |> List.fold_left
        (fun msg err -> msg ^ "\n    " ^ err) message
    in
    raise (File_test_creation_failure message')
;;

let wrap_make_test_from filename =
  try
    make_test_from filename
  with
  | File_test_creation_failure s ->
    filename >:: function _ -> assert_failure s
;;

let make_all_tests pathname =
  if Sys.file_exists pathname && Sys.is_directory pathname
  then
    Sys.files_of pathname
    |> Enum.map (fun f -> pathname ^ Filename.dir_sep ^ f)
    |> Enum.filter (fun f -> not @@ Sys.is_directory f)
    |> Enum.filter (fun f -> String.ends_with f ".code")
    |> Enum.map wrap_make_test_from
    |> List.of_enum
  else
    raise (File_test_creation_failure(
        "Test file directory " ^ pathname ^ " is missing"))
;;

let tests = "Test_files" >::: make_all_tests "test-sources";;
