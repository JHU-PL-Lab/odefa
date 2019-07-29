open Batteries;;

open Odefa_abstract_ast;;
open Odefa_adi;;
open Odefa_ast;;

open Abstract_ast;;
open Ast;;
open Adi_types;;
open Toploop_analysis_wrapper_types;;

module Make(A : Analysis) : Analysis_wrapper
  with module C = A.S.C
   and type logging_config = unit
=
struct
  module C = A.S.C;;

  let name = C.name;;

  type logging_config = unit;;

  type analysis = A.analysis * expr;;

  let create_analysis ?logging_config:(_logging_config=Some ()) expr =
    (A.analyze expr, expr)
  ;;

  let values_of_variable_from x _acl (analysis : analysis) =
    A.values_of_variable x (fst analysis)
    |> Abs_value_set.enum
    |> Enum.map
      (fun av -> Abs_filtered_value(av, Pattern_set.empty, Pattern_set.empty))
    |> Abs_filtered_value_set.of_enum
  ;;

  let contextual_values_of_variable_from x _acl ctx analysis =
    A.contextual_values_of_variable x ctx (fst analysis)
    |> Abs_value_set.enum
    |> Enum.map
      (fun av -> Abs_filtered_value(av, Pattern_set.empty, Pattern_set.empty))
    |> Abs_filtered_value_set.of_enum
  ;;

  let expression_of analysis = snd analysis;;

  let pp_analysis formatter _analysis =
    (* FIXME: printing *)
    Format.fprintf formatter "<%s analysis>" C.name
  ;;
  let show_analysis analysis =
    Jhupllib.Pp_utils.pp_to_string pp_analysis analysis
  ;;

  let get_size _analysis =
    (* FIXME: parameterize form of "size" *)
    ((-1), (-1), (-1), (-1), (-1))
  ;;
end;;
