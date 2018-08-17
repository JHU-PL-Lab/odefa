(**
   This module defines the DDPA transition functions for dynamic pops.
*)

open Batteries;;
open Jhupllib;;

open Odefa_ast;;

open Ast;;
open Ast_pp;;
open Ddpa_abstract_ast;;
open Ddpa_context_stack;;
open Ddpa_graph;;
open Ddpa_utils;;

module Make
    (C : Context_stack)
    (S : (module type of Ddpa_pds_structure_types_new.Make(C)) with module C = C)
=
struct
  module C = C;;
  module S = S;;
  open S;;

  module%continuation Targeted = struct
    [%%continue_function_name "execute"];;
    [%%continuation_type_name "t"];;
    [%%continuation_type_attributes][@@deriving eq, ord, show, to_yojson];;
    [%%continuation_data_type: (annotated_clause * C.t) option];;
    [%%continuation_data_default None];;
    let%continuation_fn targeted_dynamic_pop
        (edge : ddpa_edge) (state : S.pds_state) =
      let Ddpa_edge(
          (acl1 : annotated_clause),(acl0 : annotated_clause)
        ) = edge
      in
      let%require Program_point_state(acl0',(ctx : C.t)) = state in
      let%require true = (equal_annotated_clause acl0 acl0') in
      [%pick_lazy
         (* ********** Variable Discovery ********** *)
        (* Intermediate value *)
        begin
          let%require Continuation_value _ = [%pop Some(acl0,ctx)] in []
        end;
        (* ********** Variable Search ********** *)
        (* Value Alias *)
        begin
          let%require Unannotated_clause(uc1) = acl1 in
          let Abs_clause((x2 : abstract_var), body) = uc1 in
          let%require Abs_var_body (x1 : abstract_var) = body in
          let%require Lookup_var(x2_,patsp,patsn) = [%pop Some(acl1,ctx)] in
          let%require true = equal_abstract_var x2 x2_ in
          [Lookup_var(x1,patsp,patsn)]
        end;
        (* Stateless Clause Skip *)
        begin
          let%require Unannotated_clause(cl1) = acl1 in
          let Abs_clause((x : abstract_var),_) = cl1 in
          let (element : pds_continuation) = [%pop Some(acl1,ctx)] in
          let%require Lookup_var(x_,_,_) = element in
          let%require true = not @@ equal_abstract_var x x_ in
          let next_element = [%pop] in
          let%antirequire Deref(_,_) = next_element in
          [next_element; element]
        end;
        (* Block Marker Skip *)
        (* * This is handled as a special case because it does not involve
           a pop. *)
        (* Capture(1) *)
        begin
          let%require Continuation_value (v : abs_filtered_value) =
            [%pop Some(acl0,ctx)]
          in
          let%require Capture1 = [%pop] in
          let k1 = [%pop] in
          [Continuation_value v; k1]
        end;
        (* Capture(2) *)
        begin
          let%require Continuation_value (v : abs_filtered_value) =
            [%pop Some(acl0,ctx)]
          in
          let%require Capture2 = [%pop] in
          let (k1 : pds_continuation) = [%pop] in
          let (k2 : pds_continuation) = [%pop] in
          [Continuation_value v; k2; k1]
        end;
        (* Capture(3) *)
        begin
          let%require Continuation_value (v : abs_filtered_value) =
            [%pop Some(acl0,ctx)]
          in
          let%require Capture3 = [%pop] in
          let (k1 : pds_continuation) = [%pop] in
          let (k2 : pds_continuation) = [%pop] in
          let (k3 : pds_continuation) = [%pop] in
          [Continuation_value v; k3; k2; k1]
        end;
        (* Capture(4) *)
        begin
          let%require Continuation_value (v : abs_filtered_value) =
            [%pop Some(acl0,ctx)]
          in
          let%require Capture4 = [%pop] in
          let (k1 : pds_continuation) = [%pop] in
          let (k2 : pds_continuation) = [%pop] in
          let (k3 : pds_continuation) = [%pop] in
          let (k4 : pds_continuation) = [%pop] in
          [Continuation_value v; k4; k3; k2; k1]
        end;
        (* Capture(5) *)
        begin
          let%require Continuation_value (v : abs_filtered_value) =
            [%pop Some(acl0,ctx)]
          in
          let%require Capture5 = [%pop] in
          let (k1 : pds_continuation) = [%pop] in
          let (k2 : pds_continuation) = [%pop] in
          let (k3 : pds_continuation) = [%pop] in
          let (k4 : pds_continuation) = [%pop] in
          let (k5 : pds_continuation) = [%pop] in
          [Continuation_value v; k5; k4; k3; k2; k1]
        end;
        (* Rewind *)
        (* * This is handled in untargeted pops *)
        (* ********** Function Wiring ********** *)
        (* Function Top: Parameter Variable *)
        begin
          let%require
            Enter_clause((x:abstract_var),(x':abstract_var),c) = acl1
          in
          let%require Abs_clause(_,Abs_appl_body(_,(x3'':abstract_var))) = c in
          if not (equal_abstract_var x' x3'') then
            raise @@ Utils.Invariant_failure "Ill-formed wiring node.";
          let%require true = C.is_top c ctx in
          let ctx' = C.pop ctx in
          let%require Lookup_var(x_,patsp,patsn) = [%pop Some(acl1,ctx')] in
          let%require true = equal_abstract_var x x_ in
          [ Lookup_var(x',patsp,patsn) ]
        end;
        (* Function Bottom: Flow Check *)
        begin
          let%require Exit_clause((x : abstract_var),_,(c : abstract_clause)) = acl1 in
          let%require Abs_clause(x1'',Abs_appl_body((x2'' : abstract_var),(x3'' : abstract_var))) = c in
          if not (equal_abstract_var x x1'') then
            raise @@ Utils.Invariant_failure "Ill-formed wiring node.";
          let next_element = [%pop Some(Unannotated_clause(c),ctx)] in
          let%require Lookup_var(x_,_,_) = next_element in
          let%require true = equal_abstract_var x x_ in
          [next_element;
           Real_flow_huh;
           Jump(acl0,ctx);
           Capture2;
           Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty);
           Jump(Unannotated_clause(c),ctx);
           Lookup_var(x3'',Pattern_set.empty,Pattern_set.empty);
          ]
        end;
        (* Function Bottom: Return Variable *)
        begin
          let%require Exit_clause ((x:abstract_var),(x':abstract_var),c) =
            acl1
          in
          let%require Abs_clause(x1'',Abs_appl_body _) = c in
          if not (equal_abstract_var x x1'') then
            raise @@ Utils.Invariant_failure "Ill-formed wiring node.";
          let ctx' = C.push c ctx in
          let%require Real_flow_huh = [%pop Some(acl1,ctx')] in
          let%require Continuation_value v = [%pop] in
          let Abs_filtered_value(v',_,_) = v in
          let%require
            Abs_value_function(Abs_function_value(_,Abs_expr(acls))) = v'
          in
          let%require true = equal_abstract_var x' @@ rv acls in
          let%require Lookup_var(x_,patsp,patsn) = [%pop] in
          let%require true = equal_abstract_var x x_ in
          [ Lookup_var(x',patsp,patsn) ]
        end;
        (* Function Top: Non-Local Variable *)
        begin
          let%require Enter_clause((x'':abstract_var),x',c) = acl1 in
          let%require Abs_clause(_,Abs_appl_body((x2'':abstract_var),x3'')) =
            c
          in
          if not (equal_abstract_var x' x3'') then
            raise @@ Utils.Invariant_failure "Ill-formed wiring node.";
          let%require true = C.is_top c ctx in
          let ctx' = C.pop ctx in
          let element = [%pop Some(acl1,ctx')] in
          let%require Lookup_var(x,_,_) = element in
          let%require true = not @@ equal_abstract_var x x'' in
          [ element;
            Rewind;
            Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty)
          ]
        end;
        (* ********** Conditional Wiring ********** *)
        (* Conditional Top: Subject Positive
           Conditional Top: Subject Negative
           Conditional Top: Non-Subject Variable *)
        begin
          (* This block represents *all* conditional closure handling on
             the entering side. *)
          let%require Enter_clause((x':abstract_var),(x1:abstract_var),c) =
            acl1
          in
          let%require Abs_clause(_,Abs_conditional_body(x1',(pat:pattern),f1,_)) = c in
          if not (equal_abstract_var x1 x1') then
            raise @@ Utils.Invariant_failure "Ill-formed wiring node.";
          let Abs_function_value(f1x,_) = f1 in
          let closure_for_positive_path : bool = equal_abstract_var f1x x' in
          let element = [%pop Some(acl1,ctx)] in
          let%require Lookup_var(x,patsp,patsn) = element in
          if not (equal_abstract_var x x' || equal_abstract_var x x1) then
            [element]
          else
            let (patsp',patsn') =
              if closure_for_positive_path
              then (Pattern_set.add pat patsp,patsn)
              else (patsp,Pattern_set.add pat patsn)
            in
            [ Lookup_var(x1,patsp',patsn') ]
        end;
        (* Conditional Bottom: Return Positive
           Conditional Bottom: Return Negative *)
        begin
          let%require Exit_clause(
              (x : abstract_var),(x' : abstract_var),
              c
            ) = acl1 in
          let%require Abs_clause(
              x2,
              Abs_conditional_body((x1 : abstract_var), (pat : pattern), f1, _)
            ) = c in
          if not (equal_abstract_var x x2) then
            raise @@ Utils.Invariant_failure "Ill-formed wiring node.";
          let Abs_function_value(_,Abs_expr(cls)) = f1 in
          let f1ret = rv cls in
          let then_branch : bool = equal_abstract_var f1ret x' in
          let%require Lookup_var(x_,patsp,patsn) =
            [%pop Some(Unannotated_clause(c),ctx)]
          in
          let%require true = equal_abstract_var x x_ in
          let patsp',patsn' =
            if then_branch
            then (Pattern_set.singleton pat, Pattern_set.empty)
            else (Pattern_set.empty, Pattern_set.singleton pat)
          in
          [Lookup_var(x',patsp,patsn);
           Jump(acl1,ctx);
           Lookup_var(x1,patsp',patsn');
          ]
        end;
        (* ********** Record Construction/Destruction ********** *)
        (* Record Projection Start *)
        begin
          let%require Unannotated_clause(Abs_clause(
              (x : abstract_var),
              Abs_projection_body((x' : abstract_var),(l : ident))
            )) = acl1
          in
          let%require Lookup_var(x_,patsp,patsn) = [%pop Some(acl1,ctx)] in
          let%require true = equal_abstract_var x x_ in
          [Project(l,patsp,patsn);
           Lookup_var(x',Pattern_set.empty,Pattern_set.empty)
          ]
        end;
        (* Record Projection Stop *)
        begin
          let%require Continuation_value fv = [%pop Some(acl0,ctx)] in
          let%require Abs_filtered_value(
              Abs_value_record(r : abstract_record_value),
              (patsp0 : Pattern_set.t),
              (patsn0 : Pattern_set.t)
            ) = fv in
          let Abs_record_value (m : abstract_var Ident_map.t) = r in
          let%require Project(l,patsp1,patsn1) = [%pop] in
          let%require true = Ident_map.mem l m in
          if (not
                ((is_record_pattern_set patsp0) &&
                 [Record_pattern Ident_map.empty; Any_pattern]
                 |> List.for_all
                   (fun pattern -> (not @@ Pattern_set.mem pattern patsn0)))
             ) then
            raise @@ Utils.Invariant_failure(
              "Record projection received a value that doesn't satisfy the " ^
              "record pattern. This might be an error in the record value " ^
              "filter validation rule.");
          let%pick patsn2 = negative_pattern_set_selection_enum r patsn0 in
          let x' = Ident_map.find l m in
          let patsp' =
            Pattern_set.union patsp1 @@ pattern_set_projection patsp0 l
          in
          let patsn' =
            Pattern_set.union patsn1 @@ pattern_set_projection patsn2 l
          in
          [ Lookup_var(x',patsp',patsn') ]
        end;
        (* ********** Filter Validation ********** *)
        (* Filter Immediate *)
        begin
          let%require Unannotated_clause(
              Abs_clause((x : abstract_var),Abs_value_body (v : abstract_value))
            ) = acl1
          in
          let%require Some (pats_legal : Pattern_set.t) =
            immediately_matched_by v
          in
          let%require Lookup_var(x_,patsp,patsn) = [%pop Some(acl1, ctx)] in
          let%require true = equal_abstract_var x x_ in
          let%require true = Pattern_set.subset patsp pats_legal in
          let%require true =
            Pattern_set.is_empty @@ Pattern_set.inter patsn pats_legal
          in
          let%require true = not @@ Pattern_set.mem Fun_pattern patsn in
          let%require true = not @@ Pattern_set.mem Any_pattern patsn in
          let abs_filtered_value =
            Abs_filtered_value(v,Pattern_set.empty,Pattern_set.empty)
          in
          [ Continuation_value abs_filtered_value ]
        end;
        (* Filter Record *)
        begin
          let%require Unannotated_clause(Abs_clause(
              (x : abstract_var),
              Abs_value_body(Abs_value_record(r : abstract_record_value))
            )) = acl1
          in
          let%require Lookup_var(x_,patsp0,patsn0) = [%pop Some(acl1,ctx)] in
          let%require true = equal_abstract_var x x_ in
          let%require true = is_record_pattern_set patsp0 in
          let%require true =
            not @@ Pattern_set.mem (Record_pattern Ident_map.empty) patsn0
          in
          let%require true = not @@ Pattern_set.mem Any_pattern patsn0 in
          let%pick patsn2 = negative_pattern_set_selection_enum r patsn0 in
          let pattern_set_labels = labels_in_pattern_set patsp0 in
          let record_labels = labels_in_record r in
          let Abs_record_value(m) = r in
          let%require true =
            Ident_set.subset pattern_set_labels record_labels
          in
          let make_k'' l =
            let x'' = Ident_map.find l m in
            [ Lookup_var(x'',
                         pattern_set_projection patsp0 l,
                         pattern_set_projection patsn2 l
                        );
              Jump(acl1,ctx);
            ]
            |> List.enum
          in
          let first_pushes =
            [ Continuation_value(Abs_filtered_value(
                  Abs_value_record(r), patsp0, patsn2
                ));
              Jump(acl1,ctx);
            ]
            |> List.enum
          in
          record_labels
          |> Ident_set.enum
          |> Enum.map make_k''
          |> Enum.concat
          |> Enum.append first_pushes
          |> List.of_enum
        end;
        (* ********** State ********** *)
        (* Update Is Empty Record *)
        begin
          let%require Unannotated_clause(
              Abs_clause((x : abstract_var), Abs_update_body _)) = acl1
          in
          let%require Lookup_var(x_,patsp,patsn) = [%pop Some(acl1,ctx)] in
          let%require true = equal_abstract_var x x_ in
          let empty_record = Abs_value_record(Abs_record_value Ident_map.empty) in
          let empty_record_pat = Record_pattern Ident_map.empty in
          let%require true =
            (Pattern_set.subset patsp
               (Pattern_set.of_list [empty_record_pat; Any_pattern]))
          in
          let%require true = not @@ Pattern_set.mem empty_record_pat patsn in
          let%require true = not @@ Pattern_set.mem Any_pattern patsn in
          [ Continuation_value(Abs_filtered_value(
                empty_record,Pattern_set.empty,Pattern_set.empty))
          ]
        end;
        (* Dereference Start *)
        begin
          let%require Unannotated_clause(Abs_clause(
              (x : abstract_var), Abs_deref_body(x' : abstract_var)
            )) = acl1
          in
          let%require Lookup_var(x_,patsp,patsn) = [%pop Some(acl1,ctx)] in
          let%require true = equal_abstract_var x x_ in
          [ Deref(patsp, patsn);
            Lookup_var(x', Pattern_set.empty, Pattern_set.empty)
          ]
        end;
        (* Dereference Stop *)
        begin
          let%require Continuation_value(Abs_filtered_value(
              Abs_value_ref(cell : abstract_ref_value),patsp,patsn
            )) = [%pop Some(acl0, ctx)]
          in
          let%require true = Pattern_set.is_empty patsp in
          let%require true = Pattern_set.is_empty patsn in
          let%require Deref(patsp,patsn) = [%pop] in
          let Abs_ref_value x' = cell in
          [ Lookup_var(x', patsp, patsn) ]
        end;
        (* ********** Alias Analysis (State) ********** *)
        (* Alias Analysis Start *)
        begin
          let%require Unannotated_clause(Abs_clause(
              _, Abs_update_body((x' : abstract_var),_))) = acl1
          in
          let%require Lookup_var(
              (x : abstract_var),
              (patsp0 : Pattern_set.t),
              (patsn0 : Pattern_set.t)
            ) = [%pop Some(acl0, ctx)] in
          let deref = [%pop] in
          let%require Deref _ = deref in
          let k1'' = [ Capture5; Lookup_var(x,patsp0,patsn0) ] in
          let k2'' = [ Capture2;
                       Lookup_var(x',Pattern_set.empty,Pattern_set.empty);
                       Jump(acl1,ctx)
                     ]
          in
          let k3'' = [ Alias_huh; Jump(acl0,ctx) ] in
          let k0 = [ deref; Lookup_var(x,patsp0,patsn0) ] in
          k0 @ k3'' @ k2'' @ k1''
        end;
        (* Alias Analysis Stop *)
        begin
          let%require Unannotated_clause(Abs_clause(
              _, Abs_update_body(_, (x'' : abstract_var)))) = acl1
          in
          let%require Alias_huh = [%pop Some(acl1, ctx)] in
          let%require Continuation_value(Abs_filtered_value(
              (v : abstract_value),patsp,patsn
            )) = [%pop]
          in
          let%require true = Pattern_set.is_empty patsp in
          let%require true = Pattern_set.is_empty patsn in
          let%require Continuation_value(Abs_filtered_value(
              v',patsp,patsn
            )) = [%pop]
          in
          let%require true = Pattern_set.is_empty patsp in
          let%require true = Pattern_set.is_empty patsn in
          let (equal_values : bool) = equal_abstract_value v v' in
          let%require Lookup_var(
              (x : abstract_var),
              (patsp0 : Pattern_set.t),
              (patsn0 : Pattern_set.t)
            ) = [%pop] in
          let%require Deref(patsp1,patsn1) = [%pop] in
          if equal_values then
            [ Lookup_var(x'',patsp1,patsn1) ]
          else
            [ Deref(patsp1,patsn1); Lookup_var(x,patsp0,patsn0) ]
        end;
        (* ********** Side Effect Search (State) ********** *)
        (* Stateful Immediate Clause Skip *)
        begin
          let%require (Unannotated_clause(Abs_clause(
              (x : abstract_var),b)
            )) = acl1
          in
          let%require true = is_immediate acl1 in
          let%antirequire Abs_update_body _ = b in
          let element = [%pop Some(acl1,ctx)] in
          let%require Lookup_var(x_,_,_) = element in
          let%require true = not @@ equal_abstract_var x x_ in
          [ element ]
        end;
        (* Side Effect Search Start: Function Flow Check *)
        begin
          let%require Exit_clause(
              (x : abstract_var), _, (c : abstract_clause)
            ) = acl1
          in
          let%require Abs_clause(_,Abs_appl_body(_,_)) = c in
          let (lookup_element : pds_continuation) =
            [%pop Some(Unannotated_clause(c),ctx)]
          in
          let%require Lookup_var(x_,_,_) = lookup_element in
          let%require true = equal_abstract_var x x_ in
          let deref_element = [%pop] in
          let%require Deref(_,_) = deref_element in
          let%require Abs_clause(_,Abs_appl_body(x2'',x3'')) = c in
          [ deref_element;
            lookup_element;
            Real_flow_huh;
            Jump(acl0,ctx);
            Capture2;
            Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty);
            Jump(Unannotated_clause(c),ctx);
            Lookup_var(x3'',Pattern_set.empty,Pattern_set.empty);
          ]
        end;
        (* Side Effect Search Start: Function Flow Validated *)
        begin
          let%require Exit_clause(
              (x0'' : abstract_var), (x' : abstract_var), (c : abstract_clause)
            ) = acl1
          in
          let%require Abs_clause(_,Abs_appl_body(_,_)) = c in
          let%require Real_flow_huh = [%pop Some(acl1, C.push c ctx)] in
          let%require Continuation_value fv = [%pop] in
          let Abs_filtered_value(v,_,_) = fv in
          let%require Abs_value_function(Abs_function_value(_,e)) = v in
          let Abs_expr cls = e in
          let%require true = equal_abstract_var x' (rv cls) in
          let (lookup_element : pds_continuation) = [%pop] in
          let%require Lookup_var(x,_,_) = lookup_element in
          let%require true = not @@ equal_abstract_var x0'' x in
          let deref_element = [%pop] in
          let%require Deref(_,_) = deref_element in
          let%require Lookup_var(x,patsp,patsn) = lookup_element in
          [ deref_element;
            lookup_element;
            Side_effect_search_start;
            Side_effect_lookup_var(x,patsp,patsn,acl0,ctx)
          ]
        end;
        (* Side Effect Search Start: Conditional Positive *)
        (* Side Effect Search Start: Conditional Negative *)
        begin
          let%require Exit_clause(_,_,c) = acl1 in
          let%require Abs_clause(_,Abs_conditional_body _) = c in
          let (lookup_element : pds_continuation) = [%pop Some(acl1,ctx)] in
          let%require Lookup_var(x,_,_) = lookup_element in
          let%require Exit_clause(x_,_,_) = acl1 in
          let%require true = not @@ equal_abstract_var x x_ in
          let deref_element = [%pop] in
          let%require Deref(_,_) = deref_element in
          let%require Lookup_var(x,patsp0,patsn0) = lookup_element in
          let%require Exit_clause(_,x',c) = acl1 in
          let%require Abs_clause(_,Abs_conditional_body(x2'',p,f1,f2)) = c in
          let Abs_function_value(_,Abs_expr f1cls) = f1 in
          let Abs_function_value(_,Abs_expr f2cls) = f2 in
          let%pick (patsp',patsn') =
            if equal_abstract_var x' (rv f1cls) then
              Enum.singleton (Pattern_set.singleton p, Pattern_set.empty)
            else if equal_abstract_var x' (rv f2cls) then
              Enum.singleton (Pattern_set.empty, Pattern_set.singleton p)
            else
              Enum.empty ()
          in
          [ deref_element;
            lookup_element;
            Side_effect_search_start;
            Side_effect_lookup_var(x,patsp0,patsn0,acl0,ctx);
            Jump(Unannotated_clause c, ctx);
            Lookup_var(x2'',patsp',patsn')
          ]
        end;
        (* Side Effect Search Immediate Clause Skip *)
        begin
          let%require Unannotated_clause(Abs_clause(_,b)) = acl1 in
          let%require true = is_immediate acl1 in
          let%antirequire Abs_update_body _ = b in
          let element = [%pop Some(acl1,ctx)] in
          let%require Side_effect_lookup_var _ = element in
          [ element ]
        end;
        (* Side Effect Search: Function Bottom: Flow Check *)
        begin
          let%require Exit_clause(_,_,c) = acl1 in
          let%require Abs_clause(_,Abs_appl_body _) = c in
          let element = [%pop Some(Unannotated_clause(c),ctx)] in
          let%require Side_effect_lookup_var _ = element in
          let%require Exit_clause(_,_,c) = acl1 in
          let%require Abs_clause(_,Abs_appl_body(x2'',x3'')) = c in
          [ element;
            Real_flow_huh;
            Jump(acl0,ctx);
            Capture2;
            Lookup_var(x2'',Pattern_set.empty,Pattern_set.empty);
            Jump(Unannotated_clause(c),ctx);
            Lookup_var(x3'',Pattern_set.empty,Pattern_set.empty);
          ]
        end;
        (* Side Effect Search: Function Bottom: Flow Validated *)
        begin
          let%require Exit_clause(_,(x' : abstract_var),c) = acl1 in
          let%require Abs_clause(_,Abs_appl_body _) = c in
          let%require Real_flow_huh =
            [%pop Some(Unannotated_clause(c),C.push c ctx)]
          in
          let%require Continuation_value(Abs_filtered_value(v,_,_)) = [%pop] in
          let%require Abs_value_function(Abs_function_value(_,e)) = v in
          let Abs_expr(cls) = e in
          let%require true = equal_abstract_var x' (rv cls) in
          let element = [%pop] in
          let%require Side_effect_lookup_var _ = element in
          [ element; element ]
        end;
        (* Side Effect Search: Conditional Positive *)
        (* Side Effect Search: Conditional Negative *)
        begin
          let%require Exit_clause(_,_,c) = acl1 in
          let%require Abs_clause(_,Abs_conditional_body _) = c in
          let element = [%pop Some(Unannotated_clause(c),ctx)] in
          let%require Side_effect_lookup_var _ = element in
          let%require Exit_clause(_,x',c) = acl1 in
          let%require Abs_clause(_,Abs_conditional_body(x1,p,f1,f2)) = c in
          let Abs_function_value(_,Abs_expr f1cls) = f1 in
          let Abs_function_value(_,Abs_expr f2cls) = f2 in
          let%pick (patsp',patsn') =
            if equal_abstract_var x' (rv f1cls) then
              Enum.singleton (Pattern_set.singleton p, Pattern_set.empty)
            else if equal_abstract_var x' (rv f2cls) then
              Enum.singleton (Pattern_set.empty, Pattern_set.singleton p)
            else
              Enum.empty ()
          in
          [ element; Jump(acl1,ctx); Lookup_var(x1,patsp',patsn') ]
        end;
        (* Side Effect Search: Top *)
        begin
          let%require Enter_clause(_,_,c) = acl1 in
          let%pick ctx' =
            match c with
            | Abs_clause(_,Abs_appl_body _) -> Enum.singleton @@ C.pop ctx
            | Abs_clause(_,Abs_conditional_body _) -> Enum.singleton ctx
            | _ -> Enum.empty ()
          in
          let%require Side_effect_lookup_var _ = [%pop Some(acl1,ctx')] in
          []
        end;
        (* Side Effect Search: Complete, None Found *)
        begin
          let%require Side_effect_search_start = [%pop Some(acl0, ctx)] in
          []
        end;
        (* Side Effect Search: Alias Analysis Start *)
        begin
          let%require Unannotated_clause(Abs_clause(
              _,Abs_update_body((x' : abstract_var),_)
            )) = acl1
          in
          let element = [%pop Some(acl1,ctx)] in
          let%require Side_effect_lookup_var(x,patsp,patsn,acl',ctx') =
            element
          in
          [ element;
            Alias_huh;
            Jump(acl0,ctx);
            Capture2;
            Lookup_var(x,patsp,patsn);
            Jump(acl',ctx');
            Capture5;
            Lookup_var(x',Pattern_set.empty,Pattern_set.empty);
          ]
        end;
        (* Side Effect Search: May Not Alias *)
        begin
          let%require
            Unannotated_clause(Abs_clause(_,Abs_update_body(_,_))) = acl1
          in
          let%require Alias_huh = [%pop Some(acl1,ctx)] in
          let%require Continuation_value _ = [%pop] in
          let%require Continuation_value _ = [%pop] in
          let element = [%pop] in
          let%require Side_effect_lookup_var _ = element in
          [ element ]
        end;
        (* Side Effect Search: May Alias *)
        begin
          let%require Unannotated_clause(Abs_clause(
              _,Abs_update_body(_,(x'' : abstract_var))
            )) = acl1
          in
          let%require Alias_huh = [%pop Some(acl1,ctx)] in
          let%require Continuation_value(Abs_filtered_value(
              (v : abstract_value),_,_
            )) = [%pop] in
          let%require Continuation_value(Abs_filtered_value(
              (v' : abstract_value),_,_
            )) = [%pop] in
          let%require true = equal_abstract_value v v' in
          let%require Side_effect_lookup_var _ = [%pop] in
          [ Side_effect_search_escape x'' ]
        end;
        (* Side Effect Search: Escape: Incremental *)
        begin
          let (escape_element : pds_continuation) = [%pop Some(acl0,ctx)] in
          let%require Side_effect_search_escape _ = escape_element in
          let%require Side_effect_lookup_var _ = [%pop] in
          [ escape_element ]
        end;
        (* Side Effect Search Escape: Base *)
        begin
          let%require Side_effect_search_escape(x' : abstract_var) =
            [%pop Some(acl0,ctx)]
          in
          let%require Side_effect_search_start = [%pop] in
          let%require Lookup_var _ = [%pop] in
          let%require Deref(patsp,patsn) = [%pop] in
          [ Lookup_var(x',patsp,patsn) ]
        end;
        (* ********** Operations ********** *)
        (* Binary Operation Start *)
        begin
          let%require
            Unannotated_clause(Abs_clause(
                 (x1 : abstract_var),
                 Abs_binary_operation_body(
                   (x2 : abstract_var), _, (x3 : abstract_var)
                 )
               )) = acl1
          in
          let lookup_element = [%pop Some(acl1,ctx)] in
          let%require Lookup_var(x1_,_,_) = lookup_element in
          let%require true = equal_abstract_var x1 x1_ in
          (* The lists below are in reverse order of their presentation in the
             formal rules because we are not directly modifying the stack;
             instead, we are pushing stack elements one at a time. *)
          let eps = Pattern_set.empty in
          let k1'' = [ Capture5; Lookup_var(x2,eps,eps) ] in
          let k2'' = [ Capture2; Lookup_var(x3,eps,eps); Jump(acl1, ctx) ] in
          let k3'' = [ Binary_operation; Jump(acl0,ctx) ] in
          let k0 = [ lookup_element ] in
          k0 @ k3'' @ k2'' @ k1''
        end;
        (* Binary Operation Evaluation *)
        begin
          let%require
            Unannotated_clause(Abs_clause(
                (x1 : abstract_var),
                Abs_binary_operation_body(_, (op:binary_operator), _)
              )) = acl1
          in
          let%require Binary_operation = [%pop Some(acl1,ctx)] in
          let%require Continuation_value(Abs_filtered_value(
              (v2 : abstract_value),patsp,patsn
            )) = [%pop]
          in
          let%require true = Pattern_set.is_empty patsp in
          let%require true = Pattern_set.is_empty patsn in
          let%require
            Continuation_value(Abs_filtered_value(v1,patsp,patsn)) = [%pop]
          in
          let%require true = Pattern_set.is_empty patsp in
          let%require true = Pattern_set.is_empty patsn in
          let%require Some result_values = abstract_binary_operation op v1 v2 in
          let%pick (result_value : abstract_value) = result_values in
          let%require Lookup_var(x1_,patsp,patsn) = [%pop] in
          let%require true = equal_abstract_var x1 x1_ in
          (* NOTE: For types that are not immediate (e.g. binary operations on
             records), we'll need a different handler for pattern matching.  It
             seems that our current theory for handling binary operators only works
             for operations that return immediately matchable types. *)
          let%require Some immediate_patterns =
            immediately_matched_by result_value
          in
          let%require true = Pattern_set.subset patsp immediate_patterns in
          let%require true =
            Pattern_set.is_empty @@ Pattern_set.inter immediate_patterns patsn
          in
          [ Continuation_value(Abs_filtered_value(
                result_value, Pattern_set.empty, Pattern_set.empty
              ))
          ]
        end;
        (* Unary Operation Start *)
        begin
          let%require
            Unannotated_clause(Abs_clause(
                 (x1 : abstract_var),
                 Abs_unary_operation_body(_,(x2 : abstract_var))
               )) = acl1
          in
          let lookup_element = [%pop Some(acl1,ctx)] in
          let%require Lookup_var(x1_,_,_) = lookup_element in
          let%require true = equal_abstract_var x1 x1_ in
          let k1'' =
            [ Capture2; Lookup_var(x2,Pattern_set.empty,Pattern_set.empty) ]
          in
          let k2'' = [ Unary_operation; Jump(acl0,ctx) ] in
          let k0 = [ lookup_element ] in
          k0 @ k2'' @ k1''
        end;
        (* Unary Operation Evaluation *)
        begin
          let%require Unannotated_clause(Abs_clause(
              (x1 : abstract_var),
              Abs_unary_operation_body((op : unary_operator),_)
            )) = acl1
          in
          let%require Unary_operation = [%pop Some(acl1,ctx)] in
          let%require Continuation_value(Abs_filtered_value(
              v,patsp,patsn
            )) = [%pop]
          in
          let%require true = Pattern_set.is_empty patsp in
          let%require true = Pattern_set.is_empty patsn in
          let%require Some result_values = abstract_unary_operation op v in
          let%pick (result_value : abstract_value) = result_values in
          let%require Lookup_var(x1_,patsp,patsn) = [%pop] in
          let%require true = equal_abstract_var x1 x1_ in
          let%require Some immediate_patterns =
            immediately_matched_by result_value
          in
          let%require true = Pattern_set.subset patsp immediate_patterns in
          let%require true =
            Pattern_set.is_empty @@ Pattern_set.inter immediate_patterns patsn
          in
          [ Continuation_value(Abs_filtered_value(
                result_value, Pattern_set.empty, Pattern_set.empty
              ))
          ]
        end;
      ]
    ;;
  end;;

  (* FIXME: replace *)
  module%continuation Untargeted = struct
    [%%continue_function_name "execute"];;
    [%%continuation_type_name "t"];;
    [%%continuation_type_attributes][@@deriving eq, ord, show, to_yojson];;
    let%continuation_fn untargeted_dynamic_pop
        (eobm : End_of_block_map.t) (edge : ddpa_edge) (state : S.pds_state) =
      let Ddpa_edge(_, (acl0 : annotated_clause)) = edge in
      let%require Program_point_state(acl0_, (ctx : C.t)) = state in
      let%require true = equal_annotated_clause acl0 acl0_ in
      [%pick_lazy
         (* Jump *)
        begin
          let%require Jump(acl1, ctx) = [%pop] in
          ([], Program_point_state(acl1, ctx))
        end;
        (* Value discovery *)
        begin
          let%require Continuation_value (fv : abs_filtered_value) = [%pop] in
          let%require Bottom_of_stack = [%pop] in
          ([], Result_state(fv))
        end;
        (* Rewind *)
        begin
          (*
            To rewind, we need to know the "end-of-block" for the node we are
            considering.  We have a dictionary mapping all of the *abstract*
            clauses in the program to their end-of-block clauses, but we don't
            have such mappings for e.g. wiring nodes or block start/end nodes.
            This code runs for *every* edge, so we need to skip those cases
            for which our mappings don't exist.  It's safe to skip all
            non-abstract-clause nodes, since we only rewind after looking up
            a function to access its closure and the only nodes that can
            complete a lookup are abstract clause nodes.
          *)
          match acl0 with
          | Unannotated_clause _ ->
            begin
              match Annotated_clause_map.Exceptionless.find acl0 eobm with
              | Some(end_of_block : annotated_clause) ->
                let%require Rewind = [%pop] in
                ([], Program_point_state(end_of_block, ctx))
              | None ->
                let%require Unannotated_clause(cl0 : abstract_clause) = acl0 in
                raise @@ Utils.Invariant_failure(
                  Printf.sprintf
                    "Abstract clause lacks end-of-block mapping: %s"
                    (show_abstract_clause cl0))
            end
          | Start_clause _ | End_clause _ | Enter_clause _ | Exit_clause _ ->
            (*
              These clauses can be safely ignored because they never complete
              a lookup and so won't ever be the subject of a rewind.
            *)
            (* FIXME: currently, there is no good "stop" operation for this
               quasi-monad.  Fortunately, we can return an arbitrary value here
               (since it doesn't get used anywhere) and rely on
               create_untargeted_dynamic_pop_action_function to ignore it.
            *)
            ()
        end;
      ]
    ;;
  end;;
end;;
