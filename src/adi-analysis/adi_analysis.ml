open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_abstract_ast;;

open Abstract_ast;;
open Adi_specification;;
open Ast;;
open Logger_utils;;

let lazy_logger = make_lazy_logger "Adi_analysis";;

module type Analysis =
sig
  (** The specification for this analysis. *)
  module S : Specification
  (** A name for this analysis. *)
  val name : string
  (** The type of an analysis structure. *)
  type analysis
  (** Performs an analysis on the provided expression. *)
  val analyze : expr -> analysis
  (** Given an analysis, looks up the values of a particular variable in the
      empty context. *)
  val values_of_variable : abstract_var -> analysis -> Abs_value_set.t
  (** Given an analysis, looks up the values of a particular variable with a
      particular calling stack. *)
  val contextual_values_of_variable :
    abstract_var -> S.C.t -> analysis -> Abs_value_set.t
end;;

module Make(S : Specification) : (Analysis with module S = S) =
struct
  module S = S;;
  module T = Adi_structure_types.Make(S.C);;
  module M = Adi_monad.Make(S.C)(T);;
  module I = Adi_interpreter.Make(S)(T)(M);;

  let name = S.name;;

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
    lazy_bracket_log (lazy_logger `trace)
      (fun () ->
         Printf.sprintf "contextual_values_of_variable(%s, %s, ...)"
           (show_abstract_var x) (S.C.show ctx)
      )
      (fun result -> Abs_value_set.show result)
    @@ fun () ->
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
