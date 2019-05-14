#!/bin/bash

export LD_LIBRARY_PATH=`opam config var z3:lib`;
export DYLD_LIBRARY_PATH=`opam config var z3:lib`;