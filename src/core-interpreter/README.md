## Directory overview
Symbolic_generator is the symbolic version of the test generation algorithm. Core_interpreter_wddpac_native_2 is the concrete version. Utility functions are in Core_interpreter_utils. The other important piece is the script that runs the SAT solver, which is SATsolver.py.

## Running test generation
First we need the cfg from ddpa.

To generate graph json file run:

`./toploop_main.native --ddpa-logging=result`

Input your program and it will generate a ddpa_graphs.log.json file

If toploop_main.native is not in the repo, or you want to update ddpa, switch branches, make the ddpa binary, and then
switch branches back.

Rename the cfg json file to ddpa_graphs.json (yojson doesn't filenames with more than one period):

`cp ddpa_graphs.log.json ddpa_graphs.json`

Symbolic_generator.ml will assume the ddpa_graphs.json file is the file is wants to use.

Then run the test-generation:

`make`
`./core_toploop_main.native`

and input your program and get your input mappings.

## Restrictions
* bool variables need to be prefaced by the string 'bool'
* pattern matching is only defined on booleans
* Code expects the last line of the program to be an alias. The implementation will start lookup from the program point specified in the last line alias.

## Todo
* Be able to run benchmarks as interpreter
* Integrate inputs with benchmarks


## Problems
* z3py does not have native syntax for records. So records are implemented but
I've turned off running the SAT solver because records crash the python script.
