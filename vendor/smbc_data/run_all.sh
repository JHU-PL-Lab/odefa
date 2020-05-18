#!/usr/bin/env bash

# Run all tools on all benchmarks

export PATH=$PATH:$PWD/programs/

echo "smbc version: `smbc --version`"
echo "==== REGEX ===="
for i in regex/*.smt2 ; do \
  ./run.sh "$i" ; \
done
echo "==== PALINDROMES ===="
./run.sh palindromes/long_rev_sum1.smt2
./run.sh palindromes/long_rev_sum2.smt2
./run.sh palindromes/long_rev_sum3.smt2
echo "==== SORTED ===="
./run.sh sorted/sorted.smt2
./run.sh sorted/sorted2.smt2
./run.sh sorted/sorted3.smt2
./run.sh sorted/sorted4.smt2
echo "==== SUDOKU ===="
./run.sh sudoku/sudoku.smt2
echo "==== TYPE CHECKING ===="
./run.sh type_checking/ty_infer.smt2
./run.sh type_checking/ty_infer_nat.smt2
echo "==== EXPR ===="
./run.sh expr/expr0.smt2
./run.sh expr/expr1.smt2
./run.sh expr/expr2.smt2
./run.sh expr/expr3.smt2
echo "==== PIGEON ===="
./run.sh pigeon/pigeon4.smt2
echo "==== FOLD ===="
./run.sh fold/fold0.smt2
./run.sh fold/fold1.smt2


