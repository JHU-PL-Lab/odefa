open Parsetree;;
open Asttypes;;
open Batteries;;
open Longident;;
open Ast_helper;;
open Ocaml_ast_utils;;

type context =
  {mutable c_counter: int;
   mutable g_counter: int;
   mutable v_counter: int}

let new_context () =
  { c_counter = 0;
    g_counter = 0;
    v_counter = 0;  }
;;

let new_cont_name (c : context) =
  let n = c.c_counter in
  c.c_counter <- c.c_counter + 1;
  Lident ("Part" ^ string_of_int n)
;;

let new_goto_name (c : context) =
  let n = c.g_counter in
  c.g_counter <- c.g_counter + 1;
  Lident ("Goto" ^ string_of_int n)
;;

let new_var_name (c : context) =
  let n = c.v_counter in
  c.v_counter <- c.v_counter + 1;
  "__varct__" ^ string_of_int n
;;

type handlertype =
  | Cont_handler
  | Goto_handler
  [@@deriving eq, show]
;;

type handler =
  { h_pat : pattern;
    h_exp : expression;
    h_type : handlertype;
  }
  [@@deriving eq, show]
;;

module Handler_ord =
struct
  type t = handler
  let compare x y = Pervasives.compare x y
end;;

module Handler_set =
  Set.Make (Handler_ord);;

type handler_group =
  { back : handler;
    others : Handler_set.t [@printer Pp_utils.pp_set pp_handler Handler_set.enum]
  }
  [@@deriving eq, show]
;;

type continuation_transform_result = handler_group option * expression
  [@@deriving eq, show]
;;

let constructor_exp (name : Longident.t) (inner : expression option) =
  {pexp_desc =
     Pexp_construct (locwrap name, inner);
   pexp_loc = !default_loc;
   pexp_attributes = []}
;;

let constructor_pat (name : Longident.t) (inner : pattern option) =
  {ppat_desc =
     Ppat_construct (locwrap name, inner);
   ppat_loc = !default_loc;
   ppat_attributes = []}
;;

let rec continuation_transform
    (e : expression)
    (context : context)
  : handler_group option * expression =
  match e with
  | {pexp_desc = Pexp_ident _; _} -> (None, e)
  | {pexp_desc = Pexp_constant _; _} -> (None, e)
  | [%expr [%result [%e? r] ]] -> (None, [%expr Result [%e r]])
  | [%expr [%read]] ->
    let cont_name = new_cont_name context in
    let cont_pdesc = Ppat_construct (locwrap cont_name, None) in
    let cont_pattern = {ppat_desc = cont_pdesc; ppat_loc = !default_loc; ppat_attributes = []} in
    let next_token_exp = [%expr next_token] in
    let cont_edesc = Pexp_construct (locwrap cont_name, None) in
    let cont_exp = {pexp_desc = cont_edesc; pexp_loc = !default_loc; pexp_attributes = []} in
    let h = {h_pat = cont_pattern; h_exp = next_token_exp; h_type = Cont_handler} in
    let hgroup = Some {back = h; others = Handler_set.empty} in
    (hgroup, cont_exp)
  | {pexp_desc = Pexp_let (rflag, vblist, e2); _} ->
    (match rflag with
     | Recursive -> raise (Utils.Not_yet_implemented "Pexp_let recursive")
     | Nonrecursive ->
       (match vblist with
        | lh::[] ->
          let (hgroup1, e1') = continuation_transform lh.pvb_expr context in
          let (hgroup2, e2') = continuation_transform e2 context in
          (match hgroup1 with
           | None ->
             (hgroup2, [%expr let [%p lh.pvb_pat] = [%e e1'] in [%e e2']])
           | Some h1 ->
             let hback1 = h1.back in
             let hset1 = h1.others in
             (match hgroup2 with
              | None ->
                let new_back =
                  {hback1 with
                   h_exp = [%expr let [%p lh.pvb_pat] = [%e hback1.h_exp] in [%e e2']]} in
                let new_hgroup = Some {back = new_back; others = hset1} in
                (new_hgroup, e1')
              | Some h2 ->
                let new_hset =
                  let new_h_element =
                    {hback1 with
                     h_exp = [%expr let [%p lh.pvb_pat] = [%e hback1.h_exp] in [%e e2']]} in
                  Handler_set.union
                    (Handler_set.union (Handler_set.singleton new_h_element) hset1) h2.others in
                let new_hgroup = Some {back = h2.back; others = new_hset} in
                (new_hgroup, e1')
             )
          )
        | _ -> raise (Utils.Not_yet_implemented "Pexp_let nonrecursive multiple bindings")))
  | {pexp_desc = Pexp_function _; _} -> raise (Utils.Not_yet_implemented "Pexp_function") (*TODO*)
  | {pexp_desc = Pexp_fun _; _} -> raise (Utils.Not_yet_implemented "Pexp_fun") (*TODO*)
  | {pexp_desc = Pexp_apply _; _} -> (None, e)
  | {pexp_desc = Pexp_match _; _} -> raise (Utils.Not_yet_implemented "Pexp_match") (*TODO*)
  | {pexp_desc = Pexp_try _; _} -> raise (Utils.Not_yet_implemented "Pexp_try") (*TODO*)
  | {pexp_desc = Pexp_tuple _; _} -> (None, e)
  | {pexp_desc = Pexp_construct _; _} -> (None, e)
  | {pexp_desc = Pexp_variant _; _} -> raise (Utils.Not_yet_implemented "Pexp_variant")
  | {pexp_desc = Pexp_record _; _} -> (None, e)
  | {pexp_desc = Pexp_field _; _} -> (None, e)
  | {pexp_desc = Pexp_setfield _; _} -> raise (Utils.Not_yet_implemented "Pexp_setfield")
  | {pexp_desc = Pexp_array _; _} -> raise (Utils.Not_yet_implemented "Pexp_array")
  | {pexp_desc = Pexp_ifthenelse (e1, e2, e3_o); _} ->
    let (hgroup2_o, e2') = continuation_transform e2 context in
    let e3 =
      (match e3_o with
       | Some e3 -> e3
       | None -> [%expr ()])
    in
    let (hgroup3_o, e3') = continuation_transform e3 context in
    (match (hgroup2_o, hgroup3_o) with
     | (None, None) ->  (None, e)
     | (_, _) ->
       let goto2_name = new_goto_name context in
       let goto3_name = new_goto_name context in
       let goto4_name = new_goto_name context in
       let hgroup_others =
         (match (hgroup2_o, hgroup3_o) with
          | (None, None) -> raise (Utils.Invariant_failure "ifthenelse")
          | (Some hgroup2, None) ->
            hgroup2.others
            |> Handler_set.add
              ({h_pat = hgroup2.back.h_pat;
                h_exp = constructor_exp goto4_name (Some hgroup2.back.h_exp);
                h_type = hgroup2.back.h_type})
            |> Handler_set.add
              ({h_pat = constructor_pat goto2_name None;
                h_exp = e2';
                h_type = Goto_handler})
            |> Handler_set.add
              ({h_pat = constructor_pat goto3_name None;
                h_exp = constructor_exp goto4_name (Some e3');
                h_type = Goto_handler})
          | (None, Some hgroup3) ->
            hgroup3.others
            |> Handler_set.add
              ({h_pat = hgroup3.back.h_pat;
                h_exp = constructor_exp goto4_name (Some hgroup3.back.h_exp);
                h_type = hgroup3.back.h_type})
            |> Handler_set.add
              ({h_pat = constructor_pat goto2_name None;
                h_exp = constructor_exp goto4_name (Some e2');
                h_type = Goto_handler})
            |> Handler_set.add
              ({h_pat = constructor_pat goto3_name None;
                h_exp = e3';
                h_type = Goto_handler})
          | (Some hgroup2, Some hgroup3) ->
            hgroup3.others
            |> Handler_set.union hgroup2.others
            |> Handler_set.add
              ({h_pat = hgroup2.back.h_pat;
                h_exp = constructor_exp goto4_name (Some hgroup2.back.h_exp);
                h_type = hgroup2.back.h_type})
            |> Handler_set.add
              ({h_pat = hgroup3.back.h_pat;
                h_exp = constructor_exp goto4_name (Some hgroup3.back.h_exp);
                h_type = hgroup3.back.h_type})
            |> Handler_set.add
              ({h_pat = constructor_pat goto2_name None;
                h_exp = e2';
                h_type = Goto_handler})
            |> Handler_set.add
              ({h_pat = constructor_pat goto3_name None;
                h_exp = e3';
                h_type = Goto_handler})
         ) in
       let hgroup_back =
         let x0_name = new_var_name context in
         let x0_pat = {ppat_desc = Ppat_var (locwrap x0_name); ppat_loc = !default_loc; ppat_attributes = []} in
         {h_pat = constructor_pat goto4_name (Some x0_pat);
          h_exp = {pexp_desc = Pexp_ident (locwrap (Lident x0_name));
                   pexp_loc = !default_loc;
                   pexp_attributes = []};
          h_type = Goto_handler}
       in
       let hgroup = Some {back = hgroup_back; others = hgroup_others} in
       let new_e = [%expr if [%e e1]
                          then [%e constructor_exp goto2_name None]
                          else [%e constructor_exp goto3_name None]] in
       (hgroup, new_e))
       (*let goto2_name = new_goto_name context in
       let goto3_name = new_goto_name context in
       let goto4_name = new_goto_name context in
       let hgroup_others =
         hgroup2.others
         |> Handler_set.add
           ({h_pat = hgroup2.back.h_pat;
             h_exp = constructor_exp goto4_name (Some hgroup2.back.h_exp);
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = constructor_pat goto2_name None;
             h_exp = e2';
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = constructor_pat goto3_name None;
             h_exp = constructor_exp goto4_name (Some e3');
             h_type = Goto_handler})
       in
       let hgroup_back =
         let x0_name = new_var_name context in
         let x0_pat = {ppat_desc = Ppat_var (locwrap x0_name); ppat_loc = !default_loc; ppat_attributes = []} in
         {h_pat = constructor_pat goto4_name (Some x0_pat);
          h_exp = {pexp_desc = Pexp_ident (locwrap (Lident x0_name));
                   pexp_loc = !default_loc;
                   pexp_attributes = []};
          h_type = Goto_handler}
       in
       let hgroup = Some {back = hgroup_back; others = hgroup_others} in
       let new_e = [%expr if [%e e1]
                          then [%e constructor_exp goto2_name None]
                          else [%e constructor_exp goto3_name None]] in
       (hgroup, new_e)
     | (None, Some hgroup3) ->
       let goto2_name = new_goto_name context in
       let goto3_name = new_goto_name context in
       let goto4_name = new_goto_name context in
       let hgroup_others =
         hgroup3.others
         |> Handler_set.add
           ({h_pat = hgroup3.back.h_pat;
             h_exp = constructor_exp goto4_name (Some hgroup3.back.h_exp);
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = constructor_pat goto2_name None;
             h_exp = constructor_exp goto4_name (Some e2');
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = constructor_pat goto3_name None;
             h_exp = e3';
             h_type = Goto_handler})
       in
       let hgroup_back =
         let x0_name = new_var_name context in
         let x0_pat = {ppat_desc = Ppat_var (locwrap x0_name); ppat_loc = !default_loc; ppat_attributes = []} in
         {h_pat = constructor_pat goto4_name (Some x0_pat);
          h_exp = {pexp_desc = Pexp_ident (locwrap (Lident x0_name));
                   pexp_loc = !default_loc;
                   pexp_attributes = []};
          h_type = Goto_handler}
       in
       let hgroup = Some {back = hgroup_back; others = hgroup_others} in
       let new_e = [%expr if [%e e1]
                          then [%e constructor_exp goto2_name None]
                          else [%e constructor_exp goto3_name None]] in
       (hgroup, new_e)
     | (Some hgroup2, Some hgroup3) ->
       let goto2_name = new_goto_name context in
       let goto3_name = new_goto_name context in
       let goto4_name = new_goto_name context in
       let hgroup_others =
         hgroup3.others
         |> Handler_set.union hgroup2.others
         |> Handler_set.add
           ({h_pat = hgroup2.back.h_pat;
             h_exp = constructor_exp goto4_name (Some hgroup2.back.h_exp);
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = hgroup3.back.h_pat;
             h_exp = constructor_exp goto4_name (Some hgroup3.back.h_exp);
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = constructor_pat goto2_name None;
             h_exp = e2';
             h_type = Goto_handler})
         |> Handler_set.add
           ({h_pat = constructor_pat goto3_name None;
             h_exp = e3';
             h_type = Goto_handler})
       in
       let hgroup_back =
         let x0_name = new_var_name context in
         let x0_pat = {ppat_desc = Ppat_var (locwrap x0_name); ppat_loc = !default_loc; ppat_attributes = []} in
         {h_pat = constructor_pat goto4_name (Some x0_pat);
          h_exp = {pexp_desc = Pexp_ident (locwrap (Lident x0_name));
                   pexp_loc = !default_loc;
                   pexp_attributes = []};
          h_type = Goto_handler}
       in
       let hgroup = Some {back = hgroup_back; others = hgroup_others} in
       let new_e = [%expr if [%e e1]
                          then [%e constructor_exp goto2_name None]
                          else [%e constructor_exp goto3_name None]] in
       (hgroup, new_e)
         )*)
    (*
    let g1_name = new_goto_name context in
    let g1_pdesc = Ppat_construct (locwrap g1_name, None) in
    let g1_pat = {ppat_desc = g1_pdesc; ppat_loc = !default_loc; ppat_attributes = []} in
    let g3_name = new_goto_name context in
    let set2 =
      (match hgroup2 with
       | None ->
      ) in
    (match e3_o with
     | None ->
     | Some e3 -> raise (Utils.Not_yet_implemented "Pexp_sequence ")) *)

  | {pexp_desc = Pexp_sequence _; _} -> raise (Utils.Not_yet_implemented "Pexp_sequence") (*TODO*)
  | {pexp_desc = Pexp_while _; _} -> raise (Utils.Not_yet_implemented "Pexp_while")
  | {pexp_desc = Pexp_for _; _} -> raise (Utils.Not_yet_implemented "Pexp_for")
  | {pexp_desc = Pexp_constraint _; _} -> raise (Utils.Not_yet_implemented "Pexp_constraint")
  | {pexp_desc = Pexp_coerce _; _} -> raise (Utils.Not_yet_implemented "Pexp_coerce")
  | {pexp_desc = Pexp_send _; _} -> raise (Utils.Not_yet_implemented "Pexp_send")
  | {pexp_desc = Pexp_new _; _} -> raise (Utils.Not_yet_implemented "Pexp_new")
  | {pexp_desc = Pexp_setinstvar _; _} -> raise (Utils.Not_yet_implemented "Pexp_setinstvar")
  | {pexp_desc = Pexp_override _; _} -> raise (Utils.Not_yet_implemented "Pexp_override")
  | {pexp_desc = Pexp_letmodule _; _} -> raise (Utils.Not_yet_implemented "Pexp_letmodule")
  | {pexp_desc = Pexp_assert _; _} -> raise (Utils.Not_yet_implemented "Pexp_assert")
  | {pexp_desc = Pexp_lazy _; _} -> raise (Utils.Not_yet_implemented "Pexp_lazy")
  | {pexp_desc = Pexp_poly _; _} -> raise (Utils.Not_yet_implemented "Pexp_poly")
  | {pexp_desc = Pexp_object _; _} -> raise (Utils.Not_yet_implemented "Pexp_object")
  | {pexp_desc = Pexp_newtype _; _} -> raise (Utils.Not_yet_implemented "Pexp_newtype")
  | {pexp_desc = Pexp_pack _; _} -> raise (Utils.Not_yet_implemented "Pexp_pack")
  | {pexp_desc = Pexp_open _; _} -> raise (Utils.Not_yet_implemented "Pexp_open")
  | {pexp_desc = Pexp_extension _; _}-> raise (Utils.Not_yet_implemented "Pexp_extension")
;;
