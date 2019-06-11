open Batteries;;
open Jhupllib;;
open OUnit2;;
open Odefa_ddpa;;
open Expectation_parser_tool;;
open Odefa_toploop;;
open Odefa_ast;;

(* File testing the formatting of the analysis expectations.
   NOTE: Might not be needed in the future?
*)

(*
let all_tests =
  [ test_success "test-sources/0ddpa_addition.expectation"
  ; Test_utils.tests
  ];; *)

let () =
  let filename = "test-sources/0ddpa_addition.expectation" in
  let contents = FileUtils.read_file filename in
  let expectations =
    try
      Expectation_parser_tool.parse filename contents
    with
    | _ -> failwith "Unknown"
  in
  ()
  (* begin
    try
      let logging_instructions = Sys.getenv "LOG" in
      let parse_module_level module_level_str =
        match Logger_utils.level_of_string module_level_str with
        | Some x -> x
        | None -> failwith @@ "Invalid logging level: " ^ module_level_str
      in
      if BatString.exists logging_instructions "=" then
        let (module_name,module_level_str) =
          String.split logging_instructions ~by:"="
        in
        Logger_utils.set_logging_level_for module_name @@
          parse_module_level module_level_str
      else
        Logger_utils.set_default_logging_level @@
          parse_module_level logging_instructions
    with
    | Not_found -> ()
  end;
  run_test_tt_main ("Tests" >::: all_tests) *)
;;
