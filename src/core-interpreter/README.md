
To generate graph json file:
run ./toploop_main.native --ddpa-logging=result

This generates ddpa_graphs.log.json file

Rename it to ddpa_graphs.json (yojson doesn't like two periods in the name of the file I think)
cp ddpa_graphs.log.json ddpa_graphs.json

Symbolic_generator.ml will assume the ddpa_graphs.json file is the file is wants to use.

Restrictions:
a,b,c,d,e vars are assumed to be booleans

Notes  

Can you have a context stack that has stuff on it, but none of them are the right one?

Context annotated on variables is not really implemented at all


Let's take a bit of liberty with the pattern match formulas:
  * For boolean matches, we can just translate that to 'x=true' or something
  * We are going to assume all integer matches evaluate to true since the programmer probably messed up if you pattern matched otherwise
    - Also think we can just convert an integer pattern match to an extra binding then a bool pattern match
