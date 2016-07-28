open Parsetree;;
open Batteries;;
open Longident;;
open Ast_helper;;
open Ocaml_ast_utils;;
open Asttypes;;

type context =
  {mutable counter: int}

let new_context () =
  { counter = 0 }
;;

let new_var_name (c : context) =
  let n = c.counter in
  c.counter <- c.counter + 1;
  "var" ^ string_of_int n
;;

let string_to_exp_ident s =
  let s_desc = Pexp_ident (locwrap (Lident s)) in
  {pexp_desc = s_desc; pexp_loc = !default_loc; pexp_attributes = []}
;;

let string_to_pat_var s =
  let desc = Ppat_var (locwrap s) in
  {ppat_desc = desc; ppat_loc = !default_loc; ppat_attributes = []}
;;

(*TODO: deal with expressions inside patterns*)
let rec a_translator e c =
  let {pexp_desc; _} = e in
  match pexp_desc with
  | Pexp_ident _ -> e
  | Pexp_constant _ -> e
  | Pexp_let (rflag, vblist, e1) ->
    (match rflag with
     | Recursive -> raise (Utils.Not_yet_implemented "Pexp_let Recursive")
     | Nonrecursive ->
       let a_map vb =
         {pvb_pat = vb.pvb_pat;
          pvb_expr = a_translator vb.pvb_expr c;
          pvb_attributes = vb.pvb_attributes;
          pvb_loc = vb.pvb_loc} in
       let new_vblist = List.map a_map vblist in
       let ldesc = Pexp_let (Nonrecursive, new_vblist, a_translator e1 c) in
       {pexp_desc = ldesc; pexp_loc = !default_loc; pexp_attributes = []})
  | Pexp_function l ->
    let guardmatch case =
      (match case.pc_guard with
       | None -> None
       | Some g -> Some (a_translator g c)) in
    let casemap case =
      {pc_lhs = case.pc_lhs; pc_guard = guardmatch case; pc_rhs = a_translator case.pc_rhs c} in
    let desc = Pexp_function (List.map casemap l) in
    {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []}
  | Pexp_fun (l, e_option, p, e1) ->
    let new_e_option = (match e_option with
        | Some e0 -> Some (a_translator e0 c)
        | None -> None) in
    let new_e1 = a_translator e1 c in
    let pdesc = Pexp_fun (l, new_e_option, p, new_e1) in
    {pexp_desc = pdesc; pexp_loc = !default_loc; pexp_attributes = []}
  | Pexp_apply (e1, l) ->
    let newexp = a_translator e1 c in
    let newexp_name = new_var_name c in
    let rec loop untranslated pairlist =
      (match untranslated with
       | [] ->
         let exp_pairlist =
           List.map (fun (label, s) -> (label, (string_to_exp_ident s))) pairlist in
         let desc =
           Pexp_apply (string_to_exp_ident newexp_name, List.rev exp_pairlist) in
         {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []}
       | (lhl, lhe)::lt ->
         let e1 = a_translator lhe c in
         let v1_name = new_var_name c in
         let v1 = string_to_pat_var v1_name in
         let e2 = loop lt ((lhl, v1_name)::pairlist) in
         [%expr let [%p v1] = [%e e1] in [%e e2]])
    in
    [%expr let [%p string_to_pat_var newexp_name] = [%e newexp] in [%e loop l []]]
  | Pexp_match (e0, l) ->
    let a_e0 = a_translator e0 c in
    let a_e0_name = new_var_name c in
    let casemap case = (match case.pc_guard with
        | None ->
          {pc_lhs = case.pc_lhs; pc_guard = None; pc_rhs = a_translator case.pc_rhs c}
        | Some _ -> raise (Utils.Not_yet_implemented "Pexp_match with guard")) in
    let newcaselist = List.map casemap l in
    let desc = Pexp_match (string_to_exp_ident a_e0_name, newcaselist) in
    let newmatch = {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []} in
    [%expr let [%p string_to_pat_var a_e0_name] = [%e a_e0] in [%e newmatch]]
  | Pexp_try (e0, l) -> (*TODO: is this right? need to test*)
    let a_e0 = a_translator e0 c in
    let a_e0_name = new_var_name c in
    let casemap case = (match case.pc_guard with
        | None ->
          {pc_lhs = case.pc_lhs; pc_guard = None; pc_rhs = a_translator case.pc_rhs c}
        | Some _ -> raise (Utils.Not_yet_implemented "Pexp_try with guard")) in
    let newcaselist = List.map casemap l in
    let desc = Pexp_try (string_to_exp_ident a_e0_name, newcaselist) in
    let newtry = {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []} in
    [%expr let [%p string_to_pat_var a_e0_name] = [%e a_e0] in [%e newtry]]
  | Pexp_tuple l ->
    let rec loop e_list v_list =
      (match e_list with
       | [] ->
         let exp_v_list = List.map string_to_exp_ident v_list in
         let desc = Pexp_tuple (List.rev exp_v_list) in
         {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []}
       | lh::lt ->
         let e1 = a_translator lh c in
         let v1_name = new_var_name c in
         let v1 = string_to_pat_var v1_name in
         let e2 = loop lt (v1_name::v_list) in
         [%expr let [%p v1] = [%e e1] in [%e e2]]
      )
    in
    loop l []
  | Pexp_construct (construct, e_option) ->
    (match e_option with
     | None -> e
     | Some e1 -> let s = new_var_name c in
       let v1_p = string_to_pat_var s in
       let a_e1 = a_translator e1 c in
       let v1_e = string_to_exp_ident s in
       let c_desc = Pexp_construct (construct, Some v1_e) in
       let c_exp = {pexp_desc = c_desc; pexp_loc = !default_loc; pexp_attributes = []} in
       [%expr let [%p v1_p] = [%e a_e1] in [%e c_exp]])
  | Pexp_variant _ -> raise (Utils.Not_yet_implemented "Pexp_variant")
  | Pexp_record (plist, e_option) ->
    let rec loop untranslated pairlist translated_e_option =
      (match untranslated with
       | [] ->
         let desc = Pexp_record (List.rev pairlist, translated_e_option) in
         {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []}
       | (l, e1)::lt ->
         let a_e1 = a_translator e1 c in
         let a_e1_name = new_var_name c in
         let a_e1_pat = string_to_pat_var a_e1_name in
         let a_e1_exp = string_to_exp_ident a_e1_name in
         let newpairlist = (l, a_e1_exp)::pairlist in
         [%expr let [%p a_e1_pat] = [%e a_e1] in [%e loop lt newpairlist translated_e_option]]) in
    (match e_option with
     | None -> loop plist [] None
     | Some e0 ->
       let a_e0 = a_translator e0 c in
       let a_e0_name = new_var_name c in
       let a_e0_pat = string_to_pat_var a_e0_name in
       let a_e0_exp = string_to_exp_ident a_e0_name in
       [%expr let [%p a_e0_pat] = [%e a_e0] in [%e loop plist [] (Some a_e0_exp)]])
  | Pexp_field (e1, l) ->
    let a_e1 = a_translator e1 c in
    let a_e1_name = new_var_name c in
    let a_e1_pat = string_to_pat_var a_e1_name in
    let a_e1_exp = string_to_exp_ident a_e1_name in
    let desc = Pexp_field (a_e1_exp, l) in
    let newfield = {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []} in
    [%expr let [%p a_e1_pat] = [%e a_e1] in [%e newfield]]
  | Pexp_setfield _ -> raise (Utils.Not_yet_implemented "Pexp_setfield")
  | Pexp_array _ -> raise (Utils.Not_yet_implemented "Pexp_array")
  | Pexp_ifthenelse (e1, e2, e_o) ->
    let a_e3 =
    (match e_o with
      | None -> None
      | Some e3 -> Some (a_translator e3 c)) in
    let a_e1 = a_translator e1 c in
    let a_e1_name = new_var_name c in
    let a_e1_pat = string_to_pat_var a_e1_name in
    let a_e1_exp = string_to_exp_ident a_e1_name in
    let desc = Pexp_ifthenelse (a_e1_exp, a_translator e2 c, a_e3) in
    let ifexp = {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []} in
    [%expr let [%p a_e1_pat] = [%e a_e1] in [%e ifexp]]
  | Pexp_sequence (e1, e2) ->
    let desc = Pexp_sequence (a_translator e1 c, a_translator e2 c) in
    {pexp_desc = desc; pexp_loc = !default_loc; pexp_attributes = []}
  | Pexp_while _ -> raise (Utils.Not_yet_implemented "Pexp_while")
  | Pexp_for _ -> raise (Utils.Not_yet_implemented "Pexp_for")
  | Pexp_constraint _ -> raise (Utils.Not_yet_implemented "Pexp_constraint")
  | Pexp_coerce _ -> raise (Utils.Not_yet_implemented "Pexp_coerce")
  | Pexp_send _ -> raise (Utils.Not_yet_implemented "Pexp_send")
  | Pexp_new _ -> raise (Utils.Not_yet_implemented "Pexp_new")
  | Pexp_setinstvar _ -> raise (Utils.Not_yet_implemented "Pexp_setinstvar")
  | Pexp_override _ -> raise (Utils.Not_yet_implemented "Pexp_override")
  | Pexp_letmodule _ -> raise (Utils.Not_yet_implemented "Pexp_letmodule")
  | Pexp_assert _ -> raise (Utils.Not_yet_implemented "Pexp_assert")
  | Pexp_lazy _ -> raise (Utils.Not_yet_implemented "Pexp_lazy")
  | Pexp_poly _ -> raise (Utils.Not_yet_implemented "Pexp_poly")
  | Pexp_object _ -> raise (Utils.Not_yet_implemented "Pexp_object")
  | Pexp_newtype _ -> raise (Utils.Not_yet_implemented "Pexp_newtype")
  | Pexp_pack _ -> raise (Utils.Not_yet_implemented "Pexp_pack")
  | Pexp_open _ -> raise (Utils.Not_yet_implemented "Pexp_open")
  | Pexp_extension _ -> e

;;
