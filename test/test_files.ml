(**
   This test module will load a series of test files from the test sources
   directory and execute them one by one.

   Each file is expected to contain a comment describing the expected test
   result.  The comment should be of one of the following forms:

    NOTE: AYAKA HELPPPPPPPPP
*)

open Batteries;;
open Jhupllib;;
open OUnit2;;

open Odefa_ast;;
(* open Odefa_ddpa;; *)
open Odefa_natural;;
open Odefa_parser;;
open Odefa_toploop;;

open Ast;;
open Ast_wellformedness;;
open Odefa_abstract_ast;;
open Abstract_ast;;
open Printf;;
open Pp_utils;;
open String_utils;;
open Test_expectation_types;;
open Test_utils;;
open Toploop_options;;
open Toploop_types;;
open Toploop_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Test_files";;

type expectation_parse =
  | Success of expectation
  | Failure of string
;;

exception Expectation_parse_failure of string;;

exception Expectation_not_found;;

exception File_test_creation_failure of string;;

(* This function will pretty print general expectations and analysis specific
   expectations.
*)
let pp_test_expectation formatter expectation =
  match expectation with
  | CLExpect_evaluate -> Format.pp_print_string formatter "Expect_evaluate"
  | CLExpect_stuck -> Format.pp_print_string formatter "Expect_stuck"
  | CLExpect_well_formed -> Format.pp_print_string formatter "Expect_well_formed"
  | CLExpect_ill_formed -> Format.pp_print_string formatter "Expect_ill_formed"
  | CLExpect_consistency (analysis, consistency_expect) ->
    begin
      match consistency_expect with
      | Expect_analysis_no_inconsistencies ->
        Format.pp_print_string formatter @@
        sprintf "Expect %s to have no inconsistencies."
          (analysis_task_to_name analysis)
      | Expect_analysis_inconsistencies inconsistencies_list ->
        let incons_list =
          List.fold_left
            (fun acc -> fun inconsistency ->
               let Expect_analysis_inconsistency_at (LUVar site) = inconsistency
               in
               let res_str = acc ^ "Inconsistent at site: " ^ site ^ ";" in res_str)
            "" inconsistencies_list
        in
        let str_to_print =
          (sprintf "Expect %s to have the following inconsistencies: "
             (analysis_task_to_name analysis)) ^ incons_list
        in Format.pp_print_string formatter @@ str_to_print
    end
  | CLExpect_result res ->
    let Result (a, q, ResultString res_str) = res in
    let str_to_print = (sprintf "Expect query result for %s using %s as: "
                          (show_query q)(analysis_task_to_name a)) ^ res_str
    in Format.pp_print_string formatter @@ str_to_print
;;


(* The following set of functions will filter through the ref cell containing
   different checklist items (expectations), and change them to None if the
   expecatations of the test are in fact met. *)

let observe_evaluated expectation =
  match expectation with
  | CLExpect_evaluate -> None
  | CLExpect_stuck ->
    assert_failure @@ "Evaluation completed but was expected to become stuck."
  | _ -> Some expectation
;;

let observe_stuck failure expectation =
  match expectation with
  | CLExpect_evaluate ->
    assert_failure @@ "Evaluation became stuck: " ^ failure
  | CLExpect_stuck -> None
  | _ -> Some expectation
;;

let observe_well_formed expectation =
  match expectation with
  | CLExpect_well_formed -> None
  | CLExpect_ill_formed ->
    assert_failure @@ "Well-formedness check passed but was expect to fail."
  | _ -> Some expectation
;;

let observe_ill_formed illformednesses expectation =
  match expectation with
  | CLExpect_well_formed ->
    assert_failure @@ "Expression was unexpectedly ill-formed.  Causes:" ^
                      "\n    * " ^ concat_sep "\n    *"
                        (List.enum @@
                         List.map show_illformedness illformednesses)
  | CLExpect_ill_formed -> None
  | _ -> Some expectation
;;

let observe_queries analysis reprs expectation =
  match expectation with
  | CLExpect_result expect ->
    let Result (a, expect_q, ResultString expect_res) = expect in
    if analysis = a then
      begin
        let any_match =
          List.exists
            (fun repr ->
               (match repr with
                | QnA (actual_q, actual_res) ->
                  if (expect_q = actual_q) then
                    let pp_res_str =
                      Pp_utils.pp_to_string Abs_filtered_value_set.pp actual_res
                    in
                    (expect_res = pp_res_str)
                  else false)
            )
            reprs
        in if any_match then None else Some expectation
      end
    else Some expectation
  | _ -> Some expectation
;;

let observe_inconsistencies analysis inconsistencies expectation =
  match expectation with
  | CLExpect_consistency (a, expects) ->
    if analysis = a then
      (
        match expects with
        | Expect_analysis_inconsistencies expectations ->
          let single_inconsistency_check =
            (fun inconsistency -> fun expects ->
               let site_of_inconsistency =
                 let open Toploop_analysis_types in
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
               (List.filter_map
                  (fun expect ->
                     let Expect_analysis_inconsistency_at (LUVar expected_site) = expect
                     in
                     let ident_var = Ident expected_site in
                     if site_of_inconsistency = ident_var
                     then None
                     else Some expect)
                  expects))
          in
          let res = List.fold_left
              (fun acc -> fun single_incons -> single_inconsistency_check single_incons acc)
              expectations inconsistencies
          in
          if List.is_empty res then None
          else
            let unfinished_res = Expect_analysis_inconsistencies
                res
            in
            Some (CLExpect_consistency (a, unfinished_res))
        | Expect_analysis_no_inconsistencies -> Some expectation
      )
    else Some expectation
  | _ -> Some expectation
;;

let observe_no_inconsistency analysis expectation =
  match expectation with
  | CLExpect_consistency (a, expects) ->
    if analysis = a then
      (
        match expects with
        | Expect_analysis_inconsistencies _ -> Some expectation
        | Expect_analysis_no_inconsistencies -> None
      )
    else Some expectation
  | _ -> Some expectation
;;

(** A list of file extensions and the parser routines which are used to turn
    the contents of those files into Odefa ASTs.
*)
let test_parsers : (string * (string -> Odefa_ast.Ast.expr)) list =
  [ (".odefa",
     (fun filename -> File.with_file_in filename Parser.parse_program)
    );
    (".natodefa",
     (fun filename ->
        let on_expr = File.with_file_in filename On_parse.parse_program in
        On_to_odefa.translate on_expr
     )
    )
  ]
;;

(* This function takes in a test file, the list of expectations, and generate
   the unit test routine for it.
*)
let make_test filename (gen_expectations, analysis_expectation) =
  (* This function will print the checklist items. *)
  let name_of_checklist_item ch_item =
    match ch_item with
    | CLExpect_evaluate -> "should evaluate"
    | CLExpect_stuck -> "should get stuck"
    | CLExpect_well_formed -> "should be well-formed"
    | CLExpect_ill_formed -> "should be ill-formed"
    | CLExpect_result (result) ->
      let Result(a_task, q, ResultString(r_string)) = result in
      "should have result: " ^
      analysis_task_to_name a_task ^ ", " ^
      (string_of_query q) ^ ", " ^ r_string
    | CLExpect_consistency (analysis, consistency_expect) ->
      begin
        match consistency_expect with
        | Expect_analysis_no_inconsistencies ->
          sprintf "Expect %s to have no inconsistencies."
            (analysis_task_to_name analysis)
        | Expect_analysis_inconsistencies inconsistencies_list ->
          let incons_list =
            List.fold_left
              (fun acc -> fun inconsistency ->
                 let Expect_analysis_inconsistency_at (LUVar site) =
                   inconsistency
                 in
                 let res_str =
                   acc ^ "Inconsistent at site: " ^ site ^ ";"
                 in
                 res_str)
              "" inconsistencies_list
          in
          let str_to_print =
            (sprintf "Expect %s to have the following inconsistencies: "
               (analysis_task_to_name analysis)) ^ incons_list
          in str_to_print
      end
  in
  let checklist_expectations =
    make_checklist gen_expectations analysis_expectation in
  let test_name = filename ^ ": (" ^
                  string_of_list name_of_checklist_item
                    checklist_expectations ^ ")"
  in
  (* Create the test in a thunk. *)
  test_name >::
  function _ ->
    lazy_logger `trace (fun () ->
        Printf.sprintf "Performing test for %s with expectations: %s"
          filename
          (Pp_utils.pp_to_string (Pp_utils.pp_list pp_test_expectation)
             checklist_expectations)
      );
    (* Using a ref cell to hold not-yet-handled expectations. *)
    let expectations_left = ref checklist_expectations in
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
      let is_nato = String.ends_with filename "natodefa" in
      let expr =
        if is_nato then
          let on_expr = File.with_file_in filename On_parse.parse_program in
          On_to_odefa.translate on_expr
        else
          File.with_file_in filename Parser.parse_program in
      (* Decide what kind of analysis to perform. *)
      let analysis_list =
        let (Analysis_Expectation (_, at_list, _, _)) = analysis_expectation in
        if List.is_empty at_list then []
        else at_list
      in
      (* Configure the toploop *)
      let variables_to_analyze =
        let (Analysis_Expectation (q_list, _, _, _)) = analysis_expectation in
        if List.is_empty q_list then []
        else
          let unpacked_q_list =
            List.map (fun (Query(lookup_var, graph_pos, context)) ->
                (lookup_var, graph_pos, context)) q_list in
          unpacked_q_list
      in
      let consistency_checks =
        let (Analysis_Expectation (_, _, _, c_list)) = analysis_expectation in
        if List.is_empty c_list then AC_Map.empty
        else
          ac_tuple_list_to_dict c_list
      in
      let configuration =
        { topconf_analyses = analysis_list
        ; topconf_log_prefix = filename ^ "_"
        (* NOTE/FIXME: Commented out for easier plume testing *)
        (* ; topconf_ddpa_log_level = None
           ; topconf_pdr_log_level = None
           ; topconf_pdr_log_deltas = false
           ; topconf_graph_log_file_name = "ddpa_test.log.yojson" *)
        ; topconf_analyze_vars =
            if variables_to_analyze = []
            then Toploop_option_parsers.Analyze_no_variables
            else
              Toploop_option_parsers.Analyze_specific_variables variables_to_analyze
        ; topconf_evaluation_mode =
            (
              let has_evaluation_expectation =
                have_expectation
                  (function
                    | CLExpect_evaluate -> true
                    | CLExpect_stuck -> true
                    | _ -> false)
              in
              if has_evaluation_expectation then
                Always_evaluate
              else
                Never_evaluate
            )
        ; topconf_disable_inconsistency_check =
            AC_Map.is_empty consistency_checks
        ; topconf_disable_analysis = false
        ; topconf_report_sizes = false
        ; topconf_report_source_statistics = false
        }
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Test for %s: executing toploop handler"
            filename
        );
      (* Run the toploop *)
      let result =
        Toploop.handle_expression
          configuration
          expr
      in
      lazy_logger `trace (fun () ->
          Printf.sprintf "Test for %s: toploop result was %s"
            filename (Pp_utils.pp_to_string pp_result result)
        );
      (* Report well-formedness or ill-formedness as appropriate. *)
      lazy_logger `trace (fun () ->
          show_result result
        );
      if result.illformednesses = []
      then observation @@ observe_well_formed
      else observation @@ observe_ill_formed result.illformednesses;
      (* Report each discovered error *)
      let report = result.analysis_report in
      let keys = List.of_enum (Analysis_task_map.keys report) in
      let checking_for_one_key () (key : analysis_task) =
        let result_for_this_analysis_task =
          Analysis_task_map.find key report
        in
        let Analysis_result(qnas, errors) = result_for_this_analysis_task
        in
        observation @@ observe_inconsistencies key errors;
        (* If there are no errors, report that. *)
        if errors = [] then observation (observe_no_inconsistency key);
        (* Report each resulting variable analysis. *)
        observation @@ observe_queries key qnas;
      in
      List.fold_left checking_for_one_key () keys;

      (* Now report the result of evaluation. *)
      begin
        match result.evaluation_result with
        | Evaluation_completed _ -> observation observe_evaluated
        | Evaluation_failure failure -> observation (observe_stuck failure)
        | Evaluation_invalidated -> ()
        | Evaluation_disabled -> ()
      end;
    end;
    (* Now assert that every expectation has been addressed. *)
    match !expectations_left with
    | [] -> ()
    | expectations' ->
      assert_failure @@ "The following expectations could not be met:" ^
                        "\n    * " ^ concat_sep "\n    * "
                          (List.enum @@
                           List.map name_of_checklist_item expectations')
;;

(* This function will take in an .expectation file corresponding to a test, and
   parse the expectations in it.  It also checks for well-formedness of
   expectations: the user must give afull specification of expected query result
   for each analysis.  The returned value is a pair to be handed to the test
   generation routine.
*)
let make_expectations_from filename =
  let contents = File.with_file_in filename IO.read_all in
  let expectations =
    try
      Expectation_parser_tool.parse filename contents
    with
    | Expectation_parser_tool.ParseFailure msg ->
      raise (Expectation_parse_failure msg)
  in
  match expectations with
  | Expectations (analysis_expects, gen_expects) ->
    (match analysis_expects with
     | Some (expectation_list) ->
       (
         match expectation_list with
         | Analysis_Expectation (q_list, at_list, result_list, _) ->
           let actual_aq_set = aq_set_creation at_list q_list in
           let res_aq_list =
             List.map (fun (Result(at, qry, _)) -> (at, qry)) result_list in
           let res_aq_set = AQ_set.of_list res_aq_list in
           let coverage = AQ_set.equal actual_aq_set res_aq_set in
           if coverage
           then
             (gen_expects, expectation_list)
           else
             raise (Expectation_parse_failure "Result list is not exhaustive.
            Please provide full specification for each analysis-query pair.")
       )
     | None ->
       (gen_expects, (Analysis_Expectation([], [], [], [])))
    )
;;

let wrap_make_test_from filename =
  try
    let expectation_description = make_expectations_from filename in
    let test_extensions = List.map fst test_parsers in
    let test_filenames =
      List.filter_map
        (fun ext ->
           let candidate = Filename.remove_extension filename ^ ext in
           if Sys.file_exists candidate then Some(candidate) else None
        )
        test_extensions
    in
    match test_filenames with
    | [] ->
      raise @@ File_test_creation_failure(
        "No source file for expectation file: " ^ filename)
    | [test_filename] -> make_test test_filename expectation_description
    | _ ->
      raise @@ File_test_creation_failure(
        "Multiple source files for expectation file: " ^
        (pp_to_string (pp_list Format.pp_print_string) test_filenames))
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
    |> Enum.filter (fun f -> Filename.extension f = ".expectation")
    |> Enum.map wrap_make_test_from
    |> List.of_enum
  else
    raise (File_test_creation_failure(
        "Test file directory " ^ pathname ^ " is missing"))
;;

let tests = "Test_source_files" >::: make_all_tests "test-sources";;
