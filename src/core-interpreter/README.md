# wDDPAc interpreters
The directory contains several wDDPAc interpreters which are outlined in the paper [Practical Implementation of wDDPAc](report.pdf). All definition numbers refer to the above paper.

To run these interpreters follow the README instructions in the main odefa repo and run `./core_toploop`. Provide the specified command line arguments to specify which interpreter to run.

## Core_interpreter
Core_interpreter is a straightforward interpreter. It goes down the odefa AST and keeps matching until it hits a base case. No analysis is done in this file.

If no command line arguments are given, `./core_toploop` will run this interpreter.

## Core_interpreter_forward
Implements a traditional forward analysis. Goes through each line one at a time and continues to build up an environment until the end, and then uses the environment to say something about the return variable of the program.

No support for ref cells in forward even though there is support for them in Core_interpreter.

Use `./core_toploop -F` to run this interpreter.

## Core_interpreter_python
Converts the odefa program to Python and then prints it out, but evaluates it with the Core_interpreter.

Currently records and ref cells (at least, could be more) are not supported. This file was just to help  test the speed of Core_interpreter to a Python interpreter.

Use `./core_toploop -P` to run this interpreter.

## Core_interpreter_wddpac

Implementation of wDDPAc with no optimizations and extra support for ints, booleans, and related operations. The method initialize_graph indiscriminately adds edges for functions and conditionals, even if you cannot reach them.

Implements definition 2.1 for the core of odefa and definition 4.1 for the ints and boolean extensions.

Use `./core_toploop -W` to run this interpreter.

## Core_interpreter_wddpac_map

Map utilizes caching to remove redundant function lookups. This interpreter implements definition 2.12.

Unsigned ints not supported.

Use `./core_toploop -M` to run this interpreter.

## Core_interpreter_wddpac_my_uint

Supports unsigned integers for more efficient/lazy evaluations. Implements definition 5.1.

The interpreter Core_interpreter_wddpac_map_my_uint adds a caching mechanism described in section 5.3.

Use `./core_toploop -U` to run the interpreter without the map and `./core_toploop -U -M` to run the interpreter with the map.

## Core_interpreter_wddpac_church

Uses the church encoding to implement unsigned ints. Same definition as my_uint (5.1) but optimizations that come with using the church encoding are discussed in sections 5.1 to 5.2.

Use `./core_toploop -C` to run this interpreter.
