open Batteries;;
open Jhupllib;;

open Odefa_ast;;

let logger = Logger_utils.make_logger "Type_checker";;
let lazy_logger = Logger_utils.make_lazy_logger "Type_checker";;

exception CommandLineParseFailure of string;;
exception TypeCheckComplete;;

let get_ast (args : Type_checker_parser.type_checker_args) =
  let filename : string = args.tc_filename in
  let is_natodefa = Filename.extension filename = ".natodefa" in
  let is_odefa = Filename.extension filename = ".odefa" in
  try
    if is_natodefa then begin
      let natodefa_ast =
        File.with_file_in filename Odefa_natural.On_parse.parse_program
      in
      let odefa_ast = Odefa_natural.On_to_odefa.translate natodefa_ast
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

let () =
  let args = Type_checker_parser.parse_args () in
  let _ = get_ast args in
  ()