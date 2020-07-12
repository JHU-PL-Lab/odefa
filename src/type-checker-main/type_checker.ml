open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_natural;;
open Odefa_symbolic_interpreter;;
open Odefa_parser;;

open Odefa_answer_generation;;

let logger = Logger_utils.make_logger "Type_checker";;
let lazy_logger = Logger_utils.make_lazy_logger "Type_checker";;

exception CommandLineParseFailure of string;;
exception TypeCheckComplete;;

exception GenerationComplete;;

module Type_error_generator = Generator.Make(Generator_answer.Type_errors);;
module Ans = Type_error_generator.Answer;;

let get_ast (args : Type_checker_parser.type_checker_args)
  : (Ast.expr * Interpreter_types.abort_info Ast.Ident_map.t) =
  let filename : string = args.tc_filename in
  let is_natodefa = Filename.extension filename = ".natodefa" in
  let is_odefa = Filename.extension filename = ".odefa" in
  try
    if is_natodefa then begin
      let natodefa_ast =
        File.with_file_in filename On_parse.parse_program
      in
      let (odefa_ast, odefa_aborts) =
        On_to_odefa.translate ~is_instrumented:true natodefa_ast
      in
      Ast_wellformedness.check_wellformed_expr odefa_ast;
      (odefa_ast, odefa_aborts)
    end else if is_odefa then begin
      let odefa_ast = File.with_file_in filename Parser.parse_program in
      let () = Ast_wellformedness.check_wellformed_expr odefa_ast in
      Type_instrumentation.instrument_odefa odefa_ast
    end else begin
      raise @@ Invalid_argument "Filetype not supported"
    end
  with
  | Sys_error err ->
    begin
      prerr_endline err;
      exit 1
    end
  | Ast_wellformedness.Illformedness_found ills ->
    begin
      print_endline "Program is ill_formed.";
      let print_ill ill =
        print_string "* ";
        print_endline @@ Ast_wellformedness.show_illformedness ill;
      in
      List.iter print_ill ills;
      exit 1
    end
;;

(* TODO: Add variable of operation where type error occured *)
let () =
  let args = Type_checker_parser.parse_args () in
  let (ast, abort_map) = get_ast args in
  let abort_string = 
    abort_map
    |> Ast.Ident_map.enum
    |> Enum.map
      (fun (_, abort_info) -> Interpreter_types.show_abort_info abort_info)
    |> List.of_enum
    |> String.join "\n"
  in
  lazy_logger `debug (fun () -> Printf.sprintf "Translated program:\n%s" (Ast_pp.show_expr ast));
  lazy_logger `debug (fun () -> Printf.sprintf "Aborts:\n%s" abort_string);
  try
    let results_remaining = ref args.tc_maximum_results in
    let total_errors = ref 0 in
    let generator =
      Type_error_generator.create
        ~exploration_policy:args.tc_exploration_policy
        args.tc_generator_configuration
        abort_map
        ast
        args.tc_target_var
    in
    let generation_callback
      (type_errors : Ans.t) (steps: int) : unit =
      let _ = steps in (* Temp *)
      print_endline (Ans.show type_errors);
      flush stdout;
      total_errors := !total_errors + Ans.count type_errors;
      results_remaining := (Option.map (fun n -> n - 1) !results_remaining);
      if !results_remaining = Some 0 then begin
        raise GenerationComplete
      end;
    in
    begin
      try
        let (* answers *) _, generator_opt =
          Type_error_generator.generate_answers
            ~generation_callback:generation_callback
            args.tc_maximum_steps
            generator
        in
        (* Display number of type errors. *)
        if !total_errors = 0 then
          print_endline "No errors found."
        else
          print_endline @@ (string_of_int !total_errors) ^ " errors found.";
        (* Display if control flows have been exhausted or not. *)
        if Option.is_none generator_opt then
          print_endline "No further control flows exist."
        else
          print_endline "Further control flows may exist."
      with
      | GenerationComplete ->
        print_endline "Type errors found; terminating";
    end
  with
  | Odefa_symbolic_interpreter.Interpreter.Invalid_query msg ->
    prerr_endline msg