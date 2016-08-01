open Parsetree;;
(* open Asttypes;; *)
open Batteries;;
open Longident;;
open Ast_helper;;
open Ocaml_ast_utils;;

type context =
  {mutable c_counter: int;
   mutable g_counter: int}

let new_context () =
  { c_counter = 0;
    g_counter = 0  }
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

type handler =
  | Cont_handler of pattern * expression
  | Goto_handler of pattern * expression
  [@@deriving show]
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
  [@@deriving show]
;;

type continuation_transform_result = handler_group option * expression
  [@@deriving show]
;;

let rec continuation_transform
    (e : expression)
    (context : context)
  : handler_group option * expression =
  match e with
  | {pexp_desc = Pexp_ident _; _} -> (None, e)
  | {pexp_desc = Pexp_constant _; _} -> (None, e)
  | [%expr [%result [%e? _] ]] -> (* ([], [%expr Result [%e r]]) *)
    raise (Utils.Not_yet_implemented "[%result]")
  | [%expr [%read]] ->
    (*let constructor_name = new_constructor_name context in
    let part_pdesc = Ppat_construct (locwrap constructor_name, None) in
    let part_pattern = {ppat_desc = part_pdesc; ppat_loc = !default_loc; ppat_attributes = []} in
    let next_token_exp = [%expr next_token] in
    let part_edesc = Pexp_construct (locwrap constructor_name, None) in
    let part_exp = {pexp_desc = part_edesc; pexp_loc = !default_loc; pexp_attributes = []} in
      ([(part_pattern, next_token_exp)], part_exp) *)
    let cont_name = new_cont_name context in
    let cont_pdesc = Ppat_construct (locwrap cont_name, None) in
    let cont_pattern = {ppat_desc = cont_pdesc; ppat_loc = !default_loc; ppat_attributes = []} in
    let next_token_exp = [%expr next_token] in
    let cont_edesc = Pexp_construct (locwrap cont_name, None) in
    let cont_exp = {pexp_desc = cont_edesc; pexp_loc = !default_loc; pexp_attributes = []} in
    let h = Cont_handler (cont_pattern, next_token_exp) in
    let hgroup = Some {back = h; others = Handler_set.empty} in
    (hgroup, cont_exp)
  | {pexp_desc = Pexp_let _; _} -> (* (match rflag with
      | Recursive -> raise (Utils.Not_yet_implemented "recursive let")
      | Nonrecursive -> (match vb_list with
          | lh::[] -> (let te1 = continuation_transform lh.pvb_expr context in
                       let (l2, e2') = continuation_transform exp context in
                       match te1 with
                       | ([], e1') -> (l2, [%expr let [%p lh.pvb_pat] = [%e e1'] in [%e e2']])
                       | ((p0, e0)::l1, e1') -> let l =
                                                  ((p0, [%expr
                                                      let [%p lh.pvb_pat] = [%e e0]
                                                      in [%e e2']])::l1)@l2
                         in (l, e1')
                      )
          | _ -> raise (Utils.Not_yet_implemented "nonrecursive let with multiple value bindings"))) *)
    raise (Utils.Not_yet_implemented "Pexp_let")
  | {pexp_desc = Pexp_function _; _} -> raise (Utils.Not_yet_implemented "Pexp_function") (*TODO*)
  | {pexp_desc = Pexp_fun _; _} -> raise (Utils.Not_yet_implemented "Pexp_fun") (*TODO*)
  | {pexp_desc = Pexp_apply _; _} -> (* ([], e) *)
    raise (Utils.Not_yet_implemented "Pexp_apply")
  | {pexp_desc = Pexp_match _; _} -> raise (Utils.Not_yet_implemented "Pexp_match") (*TODO*)
  | {pexp_desc = Pexp_try _; _} -> raise (Utils.Not_yet_implemented "Pexp_try") (*TODO*)
  | {pexp_desc = Pexp_tuple _; _} -> (* ([], e) *)
    raise (Utils.Not_yet_implemented "Pexp_tuple")
  | {pexp_desc = Pexp_construct _; _} -> (* ([], e) *)
    raise (Utils.Not_yet_implemented "Pexp_construct")
  | {pexp_desc = Pexp_variant _; _} -> raise (Utils.Not_yet_implemented "Pexp_variant")
  | {pexp_desc = Pexp_record _; _} -> (* ([], e) *)
    raise (Utils.Not_yet_implemented "Pexp_record")
  | {pexp_desc = Pexp_field _; _} -> (* ([], e) *)
    raise (Utils.Not_yet_implemented "Pexp_field")
  | {pexp_desc = Pexp_setfield _; _} -> raise (Utils.Not_yet_implemented "Pexp_setfield")
  | {pexp_desc = Pexp_array _; _} -> raise (Utils.Not_yet_implemented "Pexp_array")
  | {pexp_desc = Pexp_ifthenelse _; _} -> raise (Utils.Not_yet_implemented "Pexp_ifthenelse") (*TODO*)
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
