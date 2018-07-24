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
also making unit test framework. Iota is currently not being passed up

Current restrictions:
All input statements at the beginning
Pretty flat - lookups in above levels do not continue beyond that
