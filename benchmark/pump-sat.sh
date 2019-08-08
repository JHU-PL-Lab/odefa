#!/bin/bash -e

suffix="$1"
size="$2"

if [ -z "$suffix" -o -z "$size" ]; then
    echo "You must provide a file suffix and a size."
    exit 1
fi

function make_scm {
    scheme_filename="sat-$suffix.scm"
    (
        echo "(define phi"
        for n in $(seq 1 $size); do
            echo "  (lambda (x$n)"
        done
        echo "    (and (or x1 x2) (or x1 (not x2) (not x3)) (or x3 x4) (or (not x4) x1) (or (not x2) (not x3)) (or x4 x2))"
        for n in $(seq 1 $size); do
            echo -n "  )"
        done
        echo
        echo ")"
        echo "(define try (lambda (f) (or (f #t) (f #f))))"
        echo "(define sat-solve-$size (lambda (p)"
        for n in $(seq 1 $size); do
            echo "  (try (lambda (n$n)"
        done
        echo -n "    "
        for n in $(seq 1 $size); do
            echo -n "("
        done
        echo -n "p"
        for n in $(seq 1 $size); do
            echo -n " n$n)"
        done
        echo
        echo -n "  "
        for n in $(seq 1 $size); do
            echo -n "))"
        done
        echo
        echo "))"
        echo "(sat-solve-$size phi)"
    ) > "cases/$scheme_filename"
}

function make_odefa {
    odefa_filename="sat-$suffix.natodefa"
    (
        echo -n "let phi"
        for n in $(seq 1 $size); do
            echo -n " x$n"
        done
        echo " ="
        echo "(x1 or x2) and (x1 or (not x2) or (not x3)) and (x3 or x4) and ((not x4) or x1)
  and ((not x2) or (not x3)) and (x4 or x2)"
        echo "in"
        echo "let try f ="
        echo "  (f true) or (f false)"
        echo "in"
        echo "let sat_solve_$size p ="
        for n in $(seq 1 $size); do
            echo "  try (fun n$n ->"
        done
        echo -n "    p"
        for n in $(seq 1 $size); do
            echo -n " n$n"
        done
        echo
        echo "  "
        for n in $(seq 1 $size); do
            echo -n ")"
        done
        echo
        echo "in"
        echo "sat_solve_$size phi"
    ) > "cases/$odefa_filename"
}

function make_java {
    class_name="Sat_$suffix"
    java_filename="$class_name.java"
    (
        echo "package boomerang.example;"
        echo
        echo "import java.util.function.Function;"
        echo
        echo "public class $class_name {"
        echo "    public static void main(String[] args) {"
        echo "        Phi phi = new Phi();"
        echo "        Try tryFun = new Try();"
        echo "        SatSolve$size satSolve$size = new SatSolve$size();"
        echo "        satSolve$size.tryFun = tryFun;"
        echo "        Boolean result = satSolve$size.apply(phi);"
        echo "        System.out.println(result);"
        echo "        queryFor(phi);"
        echo "        queryFor(tryFun);"
        echo "        queryFor(satSolve$size);"
        echo "        queryFor(result);"
        echo "    }"
        echo
        echo "    private static class Phi {"
        echo -n "        public Boolean apply("
        for n in $(seq 1 $size); do
            echo -n "Boolean x$n"
            [ "$n" -lt "$size" ] && echo -n ", "
        done
        echo ") {"
        echo "            boolean val = (x1 || (!x2) || (!x3)) && ((!x2) || (!x3)) && (x4 || x2);"
        echo "            Boolean result = new Boolean(val);"
        echo "            return result;"
        echo "        }"
        echo "    }"
        echo """
    private static class Try {
        public Boolean apply(Function<Boolean, Boolean> f) {
            Boolean tru = new Boolean(true);
            Boolean fal = new Boolean(false);
            Boolean res = f.apply(tru) || f.apply(fal);
            return res;
        }
    }
"""
        echo "    private static class SatSolve$size {"
        echo "        public static Try tryFun;"
        echo "        public Boolean apply(Phi phi) {"
        for n in $(seq 1 $size); do
            echo "            Function<Boolean,Boolean> f$n = new Function<Boolean,Boolean>() {"
            echo "            public Boolean apply(Boolean n$n) {"
        done
        echo -n "                boolean resVal = phi.apply("
        for n in $(seq 1 $size); do
            echo -n "n$n"
            [ "$n" -lt "$size" ] && echo -n ", "
        done
        echo ");"
        echo "                 Boolean result = new Boolean(resVal);"
        echo "                 return result;"
        for n in $(seq $size -1 1); do
            echo "             }};"
            echo "             boolean resVal = tryFun.apply(f$n);"
            echo "             Boolean result = new Boolean(resVal);"
            echo "             return result;"
        done
        echo "        }"
        echo "    }"
        echo "    private static <T> void queryFor(T query) { }"
        echo "}"
    ) > "cases/$java_filename"
}

make_scm
make_odefa
make_java
