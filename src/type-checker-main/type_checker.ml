open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Odefa_test_generation;;

let logger = Logger_utils.make_logger "Type_checker";;
let lazy_logger = Logger_utils.make_lazy_logger "Type_checker";;

exception CommandLineParseFailure of string;;
exception TypeCheckComplete;;

exception GenerationComplete;;

module Type_error_generator = Generator.Make(Generator.Type_errors);;

let get_ast (args : Type_checker_parser.type_checker_args) =
  let filename : string = args.tc_filename in
  let is_natodefa = Filename.extension filename = ".natodefa" in
  let is_odefa = Filename.extension filename = ".odefa" in
  try
    if is_natodefa then begin
      let natodefa_ast =
        File.with_file_in filename Odefa_natural.On_parse.parse_program
      in
      let odefa_ast = Odefa_natural.On_to_odefa.translate ~is_instrumented:true natodefa_ast
      in
      Ast_wellformedness.check_wellformed_expr odefa_ast;
      odefa_ast
    end else if is_odefa then begin
      if args.tc_debug then begin
        let odefa_ast =
          File.with_file_in filename Odefa_parser.Parser.parse_program
        in
        Ast_wellformedness.check_wellformed_expr odefa_ast;
        odefa_ast
      end else begin
        raise @@ Invalid_argument ".odefa files cannot be directly typechecked"
      end
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
  let ast = get_ast args in
  try
    let results_remaining = ref args.tc_maximum_results in
    let generator =
      Type_error_generator.create
        ~exploration_policy:args.tc_exploration_policy
        args.tc_generator_configuration
        ast
        args.tc_target_var
    in
    let generation_callback (type_errors : Type_error_generator.Answer.t) (steps: int) : unit =
      let _ = steps in (* Temp *)
      Printf.printf "Type errors:\n%s\n" (Type_error_generator.Answer.show type_errors);
      flush stdout;
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
        (* let answer_count = List.length answers in
        Printf.printf "%d answer%s generated\n"
          answer_count (if answer_count = 1 then "" else "s"); *)
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