open Batteries;;

open Odefa_ast;;
open Odefa_abstract_ast;;

open Abstract_ast;;
open Adi_types;;
open Ast;;

module Make(S : Specification) : (Analysis with module S = S) =
struct
  module S = S;;
  module T = Adi_structure_types.Make(S);;
  module M = Adi_monad.Make(S)(T);;
  module I = Adi_interpreter.Make(S)(T)(M);;

  type analysis = Analysis of (Ident.t -> S.C.t -> T.abstract_value Enum.t);;

  let analyze (e : expr) : analysis =
    let (_, fn) = M.run @@ I.evaluate e in
    Analysis fn
  ;;

  (** Turns an ADI-style abstract value into an Odefa-like abstract value. *)
  let simplify_abstract_value (v : T.abstract_value) : abstract_value =
    match v with
    | T.Abstract_int -> Abs_value_int
    | T.Abstract_string -> Abs_value_string
    | T.Abstract_bool b -> Abs_value_bool b
    | T.Abstract_record (m, _) ->
      Abs_value_record(Abs_record_value(Ident_map.map (fun i -> Abs_var i) m))
    | T.Abstract_function (f, _) ->
      Abs_value_function(Abstract_ast_utils.lift_function_value f)
  ;;

  let contextual_values_of_variable
      (x : abstract_var) (ctx : S.C.t) (a : analysis)
    : Abs_value_set.t =
    let Analysis fn = a in
    let Abs_var i = x in
    fn i ctx |> Enum.map simplify_abstract_value |> Abs_value_set.of_enum
  ;;
  
  let values_of_variable
      (x : abstract_var) (a : analysis)
    : Abs_value_set.t =
    contextual_values_of_variable x S.C.empty a
  ;;
end;;
