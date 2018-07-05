rules to implement:

binop
5
6

conditional
11
12
13
14
15
16

oasis setup -setup-update dynamic
./configure
ocaml setup.ml -configure --enable-tests
make

still haven't seen an example where substitution is necessary

goal: finish binop and all conditional rules by next week. Then start input guessing the week after.

attempted to change toploop so user inputs a program point to start from, but couldn't get it to work

currently doing exit clause - rules 15 and 16

current thought - just put a program point at the end to specify position. So for example:

x=1;
y=2;
....
final = x;

and to start from that node, find it in the graph and call lookup from there.
