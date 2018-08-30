oasis setup -setup-update dynamic
./configure
ocaml setup.ml -configure --enable-tests
make

still haven't seen an example where substitution is necessary


non-determinism points
binary op
input
function enter

during recursion: we lookup most recently put mapping, probably usually works but might
fail if we call the function again.

Current restrictions:
Only one input statement
Pretty flat - lookups in above levels do not continue beyond that

make recursive function test

sat solver interface
z3
or just make own module

functions with non-Unannotated_clauses inside their bodies - not sure if I properly wire those
