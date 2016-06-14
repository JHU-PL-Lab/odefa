open Ast_mapper;;
open Asttypes;;
open Parsetree;;
(*
open Ast_helper;;
open Location;;
open Longident;;
*)

let dummy_mapper _ =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension( { txt = "dummy"; _ }, payload ); _ } ->
        (match payload with
         | PStr [
             { pstr_desc = Pstr_eval({ pexp_desc = expr_desc'; _ }, attrs); _ }
           ] ->
           let expr' = { pexp_desc = expr_desc'
                       ; pexp_loc = !Ast_helper.default_loc
                       ; pexp_attributes = attrs
                       }
          in
           default_mapper.expr mapper expr'
         | PStr _ -> failwith "TODO"
         | PTyp _ -> failwith "TODO"
         | PPat (_,_) -> failwith "TODO")

      | _ ->
        default_mapper.expr mapper expr
  }
;;

let () = register "dummy" dummy_mapper;;
