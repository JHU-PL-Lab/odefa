# Benchmarks for SMBC

## Running SMBC on the Introductory Example

The problem can be found in `unrolled_example/example.smt2`.
Run:

    $ smbc unrolled_example/example.smt2

or, to list every decision and propagation made (very verbose):

    $ smbc unrolled_example/example.smt2 --debug 2

**Note**: to install smbc, you can either use `./programs/smbc` (linux 64bits)
or install it from https://github.com/c-cube/smbc/ or via `opam install smbc`.

## Requirements for the Benchmarks

- smbc https://github.com/c-cube/smbc/
- hbmc https://github.com/danr/hbmc
- CVC4 http://cvc4.cs.nyu.edu/builds/x86_64-linux-opt/unstable/
- GHC https://github.com/ghc/ghc
- Inox https://github.com/epfl-lara/inox
- TIP https://github.com/tip-org/tools
- lazysmallcheck https://hackage.haskell.org/package/lazysmallcheck
  (see script `lsc.sh` that parses problems using TIP before calling LSC)

Most of those are in `./programs/`, but LSC need to be
installed separately because HBMC will invoke the compiler and it's too big
to be included in this archive.

## Run the benchmarks

```
make all
./analyze.py
```

(the analysis script requires python3)

## Description of Content

- unrolled_example: the example detailed in the paper

- expr: simplification of arithmetic expressions
- fold: find a function to discriminate lists
- palindromes: find palindrome lists with constraints on sum/length
- regex: find regular expressions that match some strings
- sorted: find sorted lists under other constraints
- sudoku: solve a sudoku from the function that checks the result
- ty_infer: synthesize the composition operator from its type
