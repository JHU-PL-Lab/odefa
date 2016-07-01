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

let double_mapper _ =
  { default_mapper with
    expr = fun mapper expr ->
      (*
let%double x = 5 in x + 3
==>
let x = 5*2 in x + 3
*)
      match expr with
      | [%expr [%double let [%p? x]=[%e? e1] in [%e? e2]]] ->
      (* | [%expr [%double [%e?
                             {
                               Parsetree.pexp_desc = Parsetree.Pexp_let
                                   (Asttypes.Nonrecursive
                                   ,{
                                     Parsetree.pvb_pat =
                                       x;
                                     Parsetree.pvb_expr = e1;
                                     Parsetree.pvb_attributes = _;
                                     Parsetree.pvb_loc = _ }::[],e2);
                               Parsetree.pexp_loc = _;
                               Parsetree.pexp_attributes = _ }]]] -> *)
        (*mapper.expr mapper expr'*)
        let e1' = [%expr [%e e1]*2] in
        [%expr let [%p x] = [%e e1'] in [%e e2]]
      | _ ->
        default_mapper.expr mapper expr

  }
;;
