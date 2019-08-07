#!/bin/bash -e

suffix="$1"
size="$2"

if [ -z "$suffix" -o -z "$size" ]; then
    echo "You must provide a file suffix and a size."
    exit 1
fi

function make_scm {
    scheme_filename="recurse-$suffix.scm"
    (
        echo "(define (pathological x) ("
        for n in $(seq 1 $size); do
            echo "    if (eq? x x) (pathological x) ("
        done
        echo "    0"
        for n in $(seq 1 $size); do
            echo "    )"
        done
        echo "))"
        echo "(pathological 5)"
    ) > "cases/$scheme_filename"
}

function make_odefa {
    odefa_filename="recurse-$suffix.natodefa"
    (
        echo "let rec pathological x ="
        for n in $(seq 1 $size); do
            echo "    if x == x then pathological x else"
        done
        echo "    0"
        echo "in"
        echo "pathological 5"
    ) > "cases/$odefa_filename"
}

function make_java {
    class_name="Recurse_$suffix"
    java_filename="$class_name.java"
    (
        echo "package boomerang.example;"
        echo
        echo "import java.util.function.Function;"
        echo
        echo "public class $class_name {"
        echo "    public static void main(String[] args) {"
        echo "        Pathological pathological = new Pathological();"
        echo "        Integer answer = pathological.apply(new Integer(5));"
        echo "        queryFor(pathological);"
        echo "        queryFor(answer);"
        echo "    }"
        echo
        echo "    private static class Pathological {"
        echo "        public Integer apply(Integer x) {"
        for n in $(seq 1 $size); do
            echo "            if (x.intValue() == x.intValue()) { return this.apply(x); } else"
        done
        echo "            return 0;"
        echo "        }"
        echo "    }"
        echo "    private static <T> void queryFor(T query) { }"
        echo "}"
    ) > "cases/$java_filename"
}

make_scm
make_odefa
make_java
