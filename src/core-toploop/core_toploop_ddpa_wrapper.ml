open Core_ast;;
open Core_toploop_ddpa_wrapper_types;;
open Ddpa_analysis;;

module Make(A : Analysis_sig) : DDPA_wrapper =
struct
  type analysis =
    { aref : A.ddpa_analysis ref
    ; expression : expr
    };;

  let create_analysis ?logging_config:(logging_config=None) expr =
    let a =
      A.create_initial_analysis
        ~ddpa_logging_config:logging_config
        expr
    in
    { aref = ref @@ A.perform_full_closure a
    ; expression = expr
    }
  ;;

  let values_of_variable_from x acl analysis =
    let a = !(analysis.aref) in
    let (values,a') = A.values_of_variable x acl a in
    analysis.aref := a';
    values
  ;;

  let expression_of analysis = analysis.expression;;

  let pp_analysis formatter analysis =
    A.pp_ddpa_analysis formatter !(analysis.aref)
  ;;
  let show_analysis analysis =
    A.show_ddpa_analysis !(analysis.aref)
  ;;

  let get_size analysis =
    A.get_size !(analysis.aref)
  ;;
end;;
