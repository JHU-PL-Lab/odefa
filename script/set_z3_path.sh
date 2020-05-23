#!/bin/bash

# OPAM's Z3 package doesn't always seem to put the Z3 C library in a place where
# the OCaml runtime can find it.  The following should fix this on systems which
# use the LD_LIBRARY_PATH variable.
#
# Run this as: source script/set_ld_library_path
export LD_LIBRARY_PATH=`opam config var z3:lib`