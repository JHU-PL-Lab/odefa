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

You can also execute `test_generator` on specified test file. See `./test_generator -h` for details.

```
./test_generator -ttarget test-sources/input_eta.natodefa
```

In benchmark, we use the `-r1` to generate the first testcase which can reach the program point `target` with a compact output (`-btrue`) by not output the descriptive words.

```
./test_generator -ttarget -r1 -btrue benchmark-test-generation/cases/input_facehugger.natodefa
```
