oasis setup -setup-update dynamic
./configure
ocaml setup.ml -configure --enable-tests
make

still haven't seen an example where substitution is necessary


during recursion: we lookup most recently put mapping, probably usually works but might
fail if we call the function again.

conditionals inside conditionals don't get checked

Current restrictions:
Only one input statement
Pretty flat - lookups in above levels do not continue beyond that

sat solver interface
z3
or just make own module

functions with non-Unannotated_clauses inside their bodies - not sure if I properly wire those
or conditionals where the last clause is a conditional - bc I hardcode unannotated_clause


might have problems with more than one conditional because Conditional_enter_clause is not distinct


during appl case, if the last program point of function is conditional, check to see if it has been transformed, if so
find w2 and use that instead of the condiitonal

can delete all transformed_cond stuff and arg in Conditional_enter_clause if upfront transform works
