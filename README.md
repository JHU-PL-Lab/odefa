# ICFP 2020 Artifact

Name:    **[Higher-Order Demand-Driven Symbolic Evaluation]**

Authors: **[Zachary Palmer, Theodore Park, Scott Smith, Shiwei Weng]**


## Artifact Instructions

### 1. Evaluation on QEMu Instructions

The dependencies are already installed for ICFP AOE QEmu image. To run the benchmark, do

```
cd odefa
source script/set_z3_path.sh
make
make benchmark
```

The evaluation result will be in `result/evaluation-<current date>`. Besides the files for execution result and time for each testcase, the overall stat is in `0table.txt`. It will be used to fill the table in the paper.

For other usages, see Step 6 in the next section.

### 2. Build from the source

The project is tested in Ubuntu and Debian.

Step 1 - Getting the source

The source is in this submission. 

You can also grab the latest version on Github

```
git clone --recursive https://github.com/JHU-PL-Lab/odefa.git
cd odefa
git checkout icfp2020-artifact 
```

Step 2 - Install system dependencies

The required system packages include `time` and `opam`. (See `Debugging.md` or below on how to install `opam` in the QEmu image). Noting `eval $(opam env)` is automatic evaluated when you login the shell session for the next time. You need manually execute it once to run the following instruction in this shell session,

```
sudo apt update
sudo apt install time libgmp-dev

# install opam
sudo apt install opam -y

# after installing
opam init
eval $(opam env)
```

`Debugging.md`'s solution for QEmu users
```
sudo apt install mccs -y
sudo apt install opam -y
opam init --solver=mccs
eval $(opam env)
```

Step 3 - Install OCaml

```
opam switch create 4.09.0+flambda
eval $(opam env)
```

Step 4 - Install opam packages

```
# you might need to run this twice
opam install menhir batteries core gmap jhupllib monadlib ocaml-monadic pds-reachability.0.2.2 ppx_deriving ppx_deriving_yojson shexp
opam pin z3 4.8.1
opam install z3.4.8.1
```

Step 5 - Build the project

```
source script/set_z3_path.sh
make
```

Step 6 - Run the toplevel

When step 5 builds the binary `test_generator` in the project root, you can run pre-defined tasks e.g.

```
# make clean
# make
make test
make benchmark
```

You can also execute `test_generator` on specified test file. See `./test_generator -h` for option details.

```
./test_generator -h
usage: test_generator [options]

options:

  --version             show program's version and exit
  -h, --help            show this help message and exit
  -cCONTEXT_STACK, --context-stack=CONTEXT_STACK
                        Specifies the context stack used in CFG construction.
  -tVARIABLE, --target-point=VARIABLE
                        Specifies the variable to reach with generated input.
  -mMAX_STEPS, --maximum-steps=MAX_STEPS
                        Specifies the maximum number of steps to take during
                        computation.
  -rMAX_RESULTS, --maximum-results=MAX_RESULTS
                        Specifies the maximum number of results to find during
                        computation.
  -lLOG_INSTR, --log=LOG_INSTR
                        Sets the logging level.
  -ePOLICY, --exploration-policy=POLICY
  -bCOMPACT_OUTPUT, --compact-output=COMPACT_OUTPUT
                        Specifies whether the output is compact of descriptive
```

A sample toplevel execution is like

```
./test_generator -ttarget -ebfs -bfalse -r2 test-sources/input_ack_3.natodefa
Input sequence: [0, 2]
Generated in 948 steps.
Input sequence: [1, 1]
Generated in 5291 steps.
Requested input sequences found; terminating.
```

`-e` option specifies the exploration policy used in the non-deterministic lookup. Currently, it supports three policies

- bfs : breadth first
- relstack-len : smallest relative stack length first
- relstack-rep : least relative stack repetition first (default choice)

In benchmark, we use the `-r1` to generate the first testcase which can reach the program point `target` with a compact output (`-btrue`) by not output the descriptive words for numbers.

```
./test_generator -ttarget -r1 -btrue benchmark-test-generation/cases/input_facehugger.natodefa
[4]
7412
Requested input sequences found; terminating.
```
