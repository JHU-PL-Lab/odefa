open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_answer_generation;;
(* open Odefa_symbolic_interpreter.Interpreter_types;; *)

let logger = Logger_utils.make_logger "Test_generator";;
let lazy_logger = Logger_utils.make_lazy_logger "Test_generator";;

exception CommandLineParseFailure of string;;
exception GenerationComplete;;

module Input_generator = Generator.Make(Generator_answer.Input_sequence);;
module Input_sequence = Input_generator.Answer;;

let () =
  (* Parse CLI args *)
  let args = Generator_configuration_parser.parse_args () in
  (* Read the AST *)
  let (ast, aborts) =
    let is_natodefa =
      Filename.extension args.ga_filename = ".natodefa"
    in
    if is_natodefa then begin
      try
        let natast =
          File.with_file_in args.ga_filename
            Odefa_natural.On_parse.parse_program
        in
        Odefa_natural.On_to_odefa.translate natast
      with
      | Sys_error err ->
        prerr_endline err;
        exit 1
    end else begin
      try
        Odefa_natural.Type_instrumentation.instrument_odefa @@
          File.with_file_in args.ga_filename Odefa_parser.Parser.parse_program
      with
      | Sys_error err ->
        prerr_endline err;
        exit 1
    end
  in
  (* Check well-formedness of AST *)
  begin
    try
      Ast_wellformedness.check_wellformed_expr ast;
    with
    | Ast_wellformedness.Illformedness_found ills ->
      begin
        print_endline "Program is ill-formed.";
        ills
        |> List.iter
          (fun ill ->
             print_string "* ";
             print_endline @@ Ast_wellformedness.show_illformedness ill;
          );
        ignore @@ Stdlib.exit 1
      end;
  end;
  (* Generate tests *)
  try
    let results_remaining = ref args.ga_maximum_results in
    let generator = Input_generator.create
        ~exploration_policy:args.ga_exploration_policy
        args.ga_generator_configuration
        aborts
        ast
        args.ga_target_point
    in
    let failed_generation_count = ref 0 in
    let generation_callback
        (inputs : Input_sequence.t) (steps : int)
      : unit =
      if Input_sequence.generation_successful inputs then begin
        if args.ga_compact_output then begin
          Printf.printf "> %s %d\n" (Input_sequence.show inputs) steps
        end else begin
          Printf.printf "* Input sequence: %s\n* Generated in %d steps.\n"
            (Input_sequence.show inputs) steps
        end;
      end else begin
        failed_generation_count := !failed_generation_count + 1;
      end;
      flush stdout;
      results_remaining := (Option.map (fun n -> n - 1) !results_remaining);
      if !results_remaining = Some 0 then begin
        raise GenerationComplete
      end;
    in
    begin
      try
        let answers, generator_opt =
          Input_generator.generate_answers
            ~generation_callback:generation_callback
            args.ga_maximum_steps
            generator
        in
        let answer_count =
          answers
          |> List.map (fun (ans, _) ->  ans)
          |> List.flatten
          |> Input_sequence.count_list
        in
        if args.ga_compact_output then (
          Printf.printf "seq #: %d\n" answer_count;
          Printf.printf "err #: %d\n" !failed_generation_count;
          if Option.is_none generator_opt then
            print_endline "more: no"
          else
            print_endline "more: yes"
        ) else (
          Printf.printf "%d input sequence%s generated\n"
            answer_count (if answer_count = 1 then "" else "s");
          Printf.printf "%d input sequence%s failed due to errors\n"
            !failed_generation_count
            (if answer_count = 1 then "" else "s");
          if Option.is_none generator_opt then
            print_endline "No further control flows exist."
          else
            print_endline "Further control flows may exist."
        )
      with
      | GenerationComplete ->
        print_endline "Requested input sequences found; terminating.";
    end
  with
  | Odefa_symbolic_interpreter.Interpreter.Invalid_query msg ->
    prerr_endline msg
;;
