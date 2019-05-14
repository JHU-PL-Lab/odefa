# Reading `symbolic_generator.ml`

top-level definitions

`rv (cls: clause list) : var`
get the last clause in `cls`, then get the `var` of that clause

`rv_of_function (fcn:value) : var`
get `rv` of the function

`script formulas_list`
invoke python script for Z3

`make_map clauses graph : (var,clause_body) Hashtbl.t`
add `clauses` into the `graph`, which is a hashtable from `var` to function `body`

`get_annotated_clause_from_json (edge:Yojson.Basic.json list) map : annotated_clause`
used in `initialize_graph`

`initialize_graph (map: (var,clause_body) Hashtbl.t) (elements:Yojson.Basic.json list) graph: (annotated_clause,annotated_clause) Hashtbl.t`
parse a json file to a graph

`mayBeTop`
Stack API

`find_context var (node:annotated_clause) context_stack graph phi: clause Stack.t`
Stack helper API

`lookup`
core function
used in `eval_helper` and itself recursively
but not in `eval`

`eval_helper`
used in `lookup` and `eval`

`eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t * formula * input_mapping`
enter point

# Reading `core_interpreter_utils.ml`

These helper functions are used in `symbolic_generator.ml`.

type definition is used.

`find_starting_node_2`
`find_clause_by_var`
`find_by_value`
`get_non_exit_clause_node`
`matching_node`
`get_exit_nodes`
`trim_list`

Some `string_of_*` and `print_*` are used in `symbolic_generator.ml`.

# Interaction with other files
`symbolic_generator.ml` is required in the library task `core-interpreter`, together with `core_interpreter_utils.ml`.

library `core-interpreter` is required in the library task `core-toploop`, which is for executable `core_toploop`.

library `core-interpreter` is also required in the executable task `test`.

Call trace:

`core_toploop_main`.()
-> `core_toploop.handle_expression`
-> `core_toploop.do_evaluation`
-> `symbolic_generator.eval e`

# Todo

`core_toploop.ml` line 52-57
```
    if conf.topconf_wddpac_interpreter then
      (* Core_interpreter_wddpac_naive_2.eval e (* if you pass in -W you use concrete *) *)
      Symbolic_generator.eval e
    else
      (* Core_interpreter_wddpac_naive_2.eval e *)
      Symbolic_generator.eval e
```

This change is made at commit 54da563 at Feb 10 2019 when removing wddpac_2 from compilation config.
Not sure if it's work or not.