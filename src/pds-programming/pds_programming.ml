open Ast_mapper;;
open Parsetree;;
open Asttypes;;

exception Unflattened_Extension;;

module String_ord =
struct
  type t = string
  let compare x y = Pervasives.compare x y
end;;

module String_set =
  Set.Make (String_ord);;

let rec varmatch p =
  let {ppat_desc; _ } = p in
  match ppat_desc with
  | Ppat_any -> String_set.empty
  | Ppat_var s -> String_set.singleton s.txt
  | Ppat_alias (pat, s) -> String_set.union (varmatch pat) (String_set.singleton s.txt)
  | Ppat_constant _ -> String_set.empty (*what is 1L 1n etc in notes re: patterns*)
  | Ppat_interval _ -> String_set.empty
  | Ppat_tuple l -> let mapped_l = List.map varmatch l in
    List.fold_left String_set.union String_set.empty mapped_l
  | Ppat_construct (_, pat) ->
    begin match pat with
    | None -> String_set.empty
    | Some pat' -> varmatch pat'
    end
  | Ppat_variant (_, pat) ->
    begin match pat with
      | None -> String_set.empty
      | Some pat' -> varmatch pat'
    end
  | Ppat_record (l, _) -> let mapped_l =
                               let patlist = List.map (fun (_,y) -> y) l
                               in List.map varmatch patlist
    in List.fold_left String_set.union String_set.empty mapped_l
  | Ppat_array l -> let mapped_l = List.map varmatch l in
    List.fold_left String_set.union String_set.empty mapped_l
  | Ppat_or (p1, p2) -> String_set.inter (varmatch p1) (varmatch p2)
  | Ppat_constraint (p1, _) -> varmatch p1
  | Ppat_type _ -> String_set.empty
  | Ppat_lazy p1 -> varmatch p1
  | Ppat_unpack _ -> raise (Utils.Not_yet_implemented "Ppat_unpack")
  | Ppat_exception pat -> varmatch pat
  | Ppat_extension _ -> raise Unflattened_Extension;;


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

let () =
  Ppx_utils.run_mappers [dummy_mapper; double_mapper]
;;
