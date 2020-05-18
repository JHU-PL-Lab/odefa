#!/usr/bin/env bash

# Run all tools on one benchmark

FILE=$1
TIMEOUT=60
TIMEOUT_MS=$(( $TIMEOUT * 1000 ))
MEMORY=8000000
TIME_FUN="/usr/bin/time -f %E"
CGM_NAME="benchs_mem"

# limit time and memory ('v' is virtual memory, needed because 'm' ignored on linux)
ulimit -m ${MEMORY} -t ${TIMEOUT}  -Sv ${MEMORY};

# TODO
#which cgm > /dev/null || (echo "please install and enable cgmanager"; exit 1)
#
#function runmemory {
#  cgm create memory $CGM_NAME
#  cgm setvalue memory $CGM_NAME memory.limit_in_bytes 2G
#
#  echo "run \"$@\" in cgroup"
#
#  # spawn subshell to run in the cgroup
#  set +e
#  (
#  set -e
#  cgm movepid memory $CGM_NAME `sh -c 'echo $PPID'` > /dev/null
#  "$@"
#  )
#}

function runmemory {
   "$@"
}

# use cached versions of programs
export PATH=$PWD/programs/:$PATH

function line {
  echo "-----------------------"
}

echo "======================="
echo "run on ${FILE}"

line
echo "- smbc"
runmemory $TIME_FUN smbc -t ${TIMEOUT} --check -nc "${FILE}" 2>&1
if [ $? != 0 ] ; then echo "ERROR" ; fi

line
echo "- hbmc"
runmemory $TIME_FUN hbmc -q "${FILE}" 2>&1
if [ $? != 0 ] ; then echo "ERROR" ; fi

line
echo "- lazy smallcheck"
runmemory $TIME_FUN ./lsc.sh ${TIMEOUT} "${FILE}" 2>&1
if [ $? != 0 ] ; then echo "ERROR" ; fi

line
echo "- cvc4"
tip --remove-match --smtlib "${FILE}" \
  | runmemory $TIME_FUN cvc4 --lang smt --tlimit="${TIMEOUT_MS}" \
    --force-logic=ALL \
     --fmf-fun --finite-model-find --fmf-inst-engine 2>&1
if [ $? != 0 ] ; then echo "ERROR" ; fi

line
echo "- inox (z3)"
runmemory $TIME_FUN inox "${FILE}" --timeout="${TIMEOUT}" --solvers=nativez3 --checkmodels 2>&1
if [ $? != 0 ] ; then echo "ERROR" ; fi

line
echo "- inox (cvc4)"
runmemory $TIME_FUN inox "${FILE}" --timeout="${TIMEOUT}" --solvers=smt-cvc4 --checkmodels 2>&1
if [ $? != 0 ] ; then echo "ERROR" ; fi

#cgm prune memory $CGM_NAME
