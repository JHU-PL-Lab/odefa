open Parsetree;;
open Asttypes;;
open Batteries;;
open Longident;;
open Ast_helper;;

type context =
  {mutable counter: int}

let new_context () =
  { counter = 0 }
;;

let new_constructor_name (c : context) =
  let n = c.counter in
  c.counter <- c.counter + 1;
  Lident ("Part" ^ string_of_int n)
;;

let locwrap (type a) (x : a) : a Asttypes.loc =
  {txt = x;
   loc = !default_loc}
;;

let rec continuation_transform
    (e : expression)
    (context : context)
  : (pattern * expression) list * expression =
  match e with
  | {pexp_desc = Pexp_constant _; _} -> ([], e)
  | [%expr [%result [%e? r] ]] -> ([], [%expr Result [%e r]])
  | [%expr [%read]] ->
    let constructor_name = new_constructor_name context in
    let part_pdesc = Ppat_construct (locwrap constructor_name, None) in
    let part_pattern = {ppat_desc = part_pdesc; ppat_loc = !default_loc; ppat_attributes = []} in
    let next_token_exp = [%expr next_token] in
    let part_edesc = Pexp_construct (locwrap constructor_name, None) in
    let part_exp = {pexp_desc = part_edesc; pexp_loc = !default_loc; pexp_attributes = []} in
    ([(part_pattern, next_token_exp)], part_exp)
  | {pexp_desc = Pexp_let (rflag, vb_list, exp); _} -> (match rflag with
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
          | _ -> raise (Utils.Not_yet_implemented "nonrecursive let with multiple value bindings")))
  | {pexp_desc = Pexp_tuple l; _} -> (match l with
      | e1::e2::[] ->
        let ((l1, e1'),(l2, e2')) = (continuation_transform e1 context, continuation_transform e2 context)
        in (match (l1, l2) with
            | ([],[]) -> ([],[%expr ([%e e1'],[%e e2'])])
            | _ -> raise (Utils.Not_yet_implemented "Pexp_tuple with [%read]") )
      | _ -> raise (Utils.Not_yet_implemented "Pexp_tuple: not a pair"))
  | _ -> raise (Utils.Not_yet_implemented "continuation transform")
;;
