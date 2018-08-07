oasis setup -setup-update dynamic
./configure
ocaml setup.ml -configure --enable-tests
make

still haven't seen an example where substitution is necessary

x=1;
y=2;
....
final = x;

and to start from that node, find it in the graph and call lookup from there.

non-determinism points
binary op
input
function enter

first attempt at successor, haven't tested at all

currently working on: when starting from inside conditional, sometimes previous node doesn't exist - change starting node method.

Current restrictions:
All input statements at the beginning
Pretty flat - lookups in above levels do not continue beyond that



sat solver interface
z3
or just make own module

functions with non-Unannotated_clauses inside their bodies - not sure if I properly wire those

x = input;
y = 2;
z = x == y;
w = z ~ true ? fun n-> (b=n) : fun c-> (d=c);
q=b;
;;

seems like rule 15 is not correct
