open Ast_mapper;;

let dummy_mapper _ =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | [%expr [%dummy [%e? expr']]] ->
        mapper.expr mapper expr'
      | _ ->
        default_mapper.expr mapper expr
  }
;;

let () = register "dummy" dummy_mapper;;
