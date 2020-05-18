#!/usr/bin/env bash

# script for Lazy Small Check
# use: `lsc.sh timeout file.smt2`

TIP_FILE=$2
TIMEOUT=$1

BASENAME=`basename ${TIP_FILE} .smt2`
ROOT=/tmp/lsc/`dirname ${TIP_FILE}`/
mkdir -p $ROOT
HS_FILE=$ROOT/$BASENAME.hs
BINARY=$ROOT/$BASENAME

tip --haskell-lazysc $TIP_FILE > $HS_FILE
ulimit -t ${TIMEOUT} ; runghc $HS_FILE



