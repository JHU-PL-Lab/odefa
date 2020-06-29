# ICFP 2020 Artifact

Name:    **[Higher-Order Demand-Driven Symbolic Evaluation]**

Authors: **[Zachary Palmer, Theodore Park, Scott Smith, Shiwei Weng]**


## Project Structure

This is brief description of the directory structure related to this artifact.

```
.
├── benchmark-test-generation       
│   ├── benchmark.ml                the benchmark script 
│   └── cases                       the benchmarks from SMBC and Scheme benchmarks mentioned by the paper
├── result                          benchmark running result
├── src                             the source code of the tool
├── test                            the source code for testing
├── test-sources                    unit test and other tests
└── vendor                          the original repositories of benchmark suite for referencing
```

## Artifact Instructions

### 1. Evaluation on QEMu Instructions

The dependencies are already installed for ICFP AOE QEmu image. To run the benchmark, do

```
cd odefa
make
make benchmark
```

The evaluation result will be in `result/evaluation-<current date>`. For each benchmark `<test-name>`, the output is in `<test-name>_<n>.txt` for n-th execution and the recorded execution time is in `<test-name>.time.txt`. The overall statistics is in `0table.txt`. It will be used to fill the table in the paper. The evaluation data for this paper is in `result/paper-evaluate-<date>` while the previous used data is in `result/archive`. Case `long_rev_sum3` here corresponds to case `palindrome` in the paper evaluation.

For other usages, see Step 6 below and toplevel usage section.

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

The required system packages include `time` and `opam`. (See `Debugging.md` or below on how to install `opam` in the QEmu image). Noting `eval $(opam env)` is automatic evaluated when you login the shell session for the next time. You need manually execute it once to run the following instruction in this shell session.

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
# you might need to run this twice if not installed successfully.
opam install menhir batteries core gmap ppx_deriving ppx_deriving_yojson shexp
opam install jhupllib monadlib ocaml-monadic pds-reachability.0.2.2 z3

# For QEmu image, you might need to free up some space when you see error messages on disk space. 
opam clean
```

Step 5 - Build the project

```
make
```

Step 6 - Run the toplevel

When step 5 builds the binary `test_generator` in the project root, you can run pre-defined targets in `Makefile` e.g.

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

A sample toplevel execution is

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

## Toplevel Usage

### Natodefa syntax

The toplevel `test_generator` supports test files in two kind of syntax, determined by the filename extension - `.natodefa` and `.odefa`. (`.oedfa` uses the ANF grammar mentioned in Figure 5 of the paper, which may be cumbersome to write directly.) The `natodefa` uses the common functional language syntax with support of integers, booleans, records and functions, which is more natural to write.

This is the concrete syntax of the `natodefa`. See `benchmark-test-generation/cases/<test-case>.natodefa` and `test-sources/<test-case>.natodefa` for more examples.

```
x ::= (identifier)                    (* alphanumeric starting with letter *)
l ::= (label)                         (* alphanumeric starting with letter *)

comment 
  ::= # (any char)* '\n'              (* in line comment *)

e ::= 0 | 1 | (-1) | ...              (* integer literal *)
    | e + e | e - e | e * e | e / e   (* integer arithmetics *)
    | e < e | e <= e | e == e         (* integer compare operations *)

    | true | false                    (* boolean literal *)
    | e and e | e or e | not e        (* boolean operations *)
    | if e then e else e              (* condition *)

    | {l_1 = e_1, ... ,l_n = e2}      (* record *)
    | e.l                             (* record projection *)

    | input                           (* integer input from the world *)

    | fun x_1 ... x_n -> e            (* anonymous function *)
    | let x = e in e                  (* let binding of one id *)
    | let f x_1 ... x_n = e in e      (* let binding of a non-recursive function *)
    | let rec f x_1 ... x_n = e in e  (* let binding of a recursive function *)

    | (e)                             (* parenthesis *)
```

### Example - Hello

The example file `hello.natodefa` is

```
let x = input in
let f t = t in
let rec sum x =
  if x == 0 then
    0
  else
    x + sum (x-1)
in
if (f x) + 1 == sum 3 and true then
  let target = 1 in 1
else
  0
```

The program takes an `input` as x and checks whether some operations on `x` can make it equal to `sum 3`.

A program usually starts with several `input`s, denoting taking input from the world. Though as a test generator you don't need to input the numbers yourself, the expected input will be generated depending on your target program point. When running with this command

```
$ ./test_generator hello.natodefa -t target -r 1

Input sequence: [5]
Generated in 2275 steps.
Requested input sequences found; terminating.
```

The output reads: with the input sequence `5` for `hello.natodefa` (consumed at line 1), it can reach `target` at line 10.

### Example - Sum

The example file `sum.natodefa` is

```
let x = input in
if 0 < x then
  let rec sum prev acc = 
    let t = input in
    if t == 0 then
      acc
    else if prev < t then
      sum t (acc + t)
    else 
      let hidden = 1 in 0
  in
  if 10 * 3 / 2 == sum x 0 then
    let target = 1 in 1
  else
    0
else
  0
```

This program takes an initial input as `x`. When `x` is positive, it takes `input` whenever it's greater than the previous one and accumulates it. Finally, it checks the sum of them.

When running with this command

```
$ ./test_generator sum.natodefa -t target -r 3

Input sequence: [1, 15, 0]
Generated in 1753 steps.
Input sequence: [6, 7, 8, 0]
Generated in 2753 steps.
Input sequence: [1, 3, 4, 8, 0]
Generated in 3783 steps.
Requested input sequences found; terminating.
```

The output reads: the test generator finds three(`3`) possible input sequences (with three difference executions). If the program runs with any of the input sequence, it can reach the program point `target`.

### Example - Sum (hidden)

When running with this command

```
$ ./test_generator sum.natodefa -t hidden -r 2

Input sequence: [13, 1]
Generated in 1431 steps.
Input sequence: [6, 7, 1]
Generated in 2207 steps.
Requested input sequences found; terminating.
```

For this command, we are interested any input sequence which let the program reach the program point `hidden` inside the function `sum` no matter where it's called. Running the program with any input sequence in the output results in a concrete execute in which we can reach `hidden`.

## Unit Testing

The unit testing is not intended to used by users and is mainly used in developing. We give a brief description for completeness.

`make test` runs all unit testings. The directory `test` includes unit testing for different modules. `test/files.ml` tests all immediate files in `test-sources`. Test files follow the same syntax describe above, with an extra prologue section for testing expectation in each file. The concrete syntax is:

```
e ::= (defined before)

number
  ::= 0 | 1 | -1 | ...

expectation 
  ::= # EXPECT-WELL-FORMED                                      (* expect syntactic correctness *)
    | # EXPECT-INPUT-SEQUENCES-REACH x [ number* ]              (* expect to reach program point `x` with input sequence `number`s *)
    | # EXPECT-REQUIRED-INPUT-SEQUENCE-GENERATION-STEPS number  (* expect running steps, throw exception if it exceeds. Default to 10000  *)
    | ...                                                       (* other expectations for the program analysis step *)    

p ::= {expectation '\n'}* e
```

Noting `#` is also the syntax for in line comment. Therefore, these expectation will be ignored when directly running the toplevel with test files in the `test-sources` directory.