open Odefa_ast;;

open Ast;;

type type_checker_args = {
  tc_filename : string;
  tc_target_var : Ident.t;
  tc_debug : bool;
}
;;

val parse_args : unit -> type_checker_args;;