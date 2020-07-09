open Odefa_ast;;

type odefa_abort_info =
  {
      (* odefa_abort_type : Abort_type (* new variant *) *)
      odefa_abort_symbol : Ast.var;
      odefa_abort_matches : Ast.clause list;
      odefa_abort_operation : Ast.clause;
  }
  [@@deriving eq, ord]
;;

type odefa_natodefa_info =
	{
	  (* odefa_natodefa_vars : On_ast.var Ast.Var_map.t;   (* ??? *) *)
	  (* odefa_natodefa_exprs : On_ast.expr Ast.Var_map.t; (* ??? *) *)
	  mutable odefa_aborts : odefa_abort_info Ast.Var_map.t;
    mutable natodefa_exprs : On_ast.expr Ast.Var_map.t;
	  (* natodefa_locations : (int * int) Map(On_ast.expr) *)
	}
  [@@deriving eq, ord]
;;