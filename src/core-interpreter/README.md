
To generate graph json file:
run ./toploop_main.native --ddpa-logging=result

This generates ddpa_graphs.log.json file

Rename it to ddpa_graphs.json (yojson doesn't like two periods in the name of the file I think)
cp ddpa_graphs.log.json ddpa_graphs.json

Symbolic_generator.ml will assume the ddpa_graphs.json file is the file is wants to use.



Notes  

lookup_stack is a (var * string) stack. What does the string represent? The context? if so why is it not just
(var * annotated_clause) stack (or whatever type context is)?

Didn't test negated - ddpa didn't like true

Not sure what the purpose of context stack is now if we just annotated every var

Just finished exit clause - not completely sure it's right
working on parsing enter and exit clauses from ddpa cfg next
