#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

# Requirements:
#   * P4F prototype artifact cloned to ../local/p4f
#   * boomerang-artifact repo cloned to ../local/boomerang-artifact
#   * SPDS-experiements repo cloned to ../local/SPDS-experiments (on popl-aec branch)
#     * With appropriate "git submodule init" and "git submodule update"
#   * Java 8, ANT, and Maven in PATH (as Soot needs rt.jar to exist)
#   * Scala and sbt in PATH

echo "START"

declare -A CASES=(
  [ack]=1
  [blur]=1
  # [church]=1 # Triggers P4F implementation bug. P4F throws exception if non-function appears as operator. Minimal working example: ‘((if (< 1 2) (lambda () 'anything) #t))’.
  [cpstak]=1
  [deriv]=1
  [eta]=1
  [facehugger]=1
  [flatten]=1
  [kcfa-2]=5
  [kcfa-3]=7
  [loop2-1]=1
  [map]=1
  [mj09]=1
  [primtest]=1
  [regex]=1
  [rsa]=1
  [sat-1]=4
  [sat-2]=14
  [sat-3]=14
 # [state]=1 # Boxes aren’t supported by P4F.
  [tak]=1
#)
#declare -A CASES=(
  [sat-P4]=1
  [sat-P6]=1
  [sat-P8]=1
  [sat-P10]=1
  [sat-P12]=1
  [sat-P14]=1
  [sat-P16]=1
  [sat-P18]=1
  [sat-P20]=1
  [sat-P22]=1
  [sat-P24]=1
#)
#declare -A CASES=(
  [recurse-S2]=1
  [recurse-S3]=1
  [recurse-S4]=1
  [recurse-S5]=1
  [recurse-S6]=1
  [recurse-S7]=1
  [recurse-S8]=1
)

TRIALS=10
TIMEOUT=30m
HERE="$(cd "$(dirname $0)" && pwd)"
CASES_PATH="${HERE}/cases"
RESULTS_PATH="${HERE}/results"
RESOURCES_PATH="${HERE}/resources"

ODEFA="${HERE}/.."
ODEFA_TOPLOOP="${ODEFA}/toploop"
NATODEFA_TRANSLATOR="${ODEFA}/translator"

function findLocalDir {
    dirname="$1"
    progname="$2"
    if [ -d "${ODEFA}/local/$dirname" ]; then
        echo "${ODEFA}/local/$dirname"
    elif [ -d "${ODEFA}/../$dirname" ]; then
        echo "${ODEFA}/../$dirname"
    else
        echo "$progname not found!" >&2;
        exit 1
    fi
}

P4F="$(findLocalDir "p4f" "P4F prototype")"
P4F_CLASSPATH="${P4F}/target/scala-2.11/classes"
P4F_STATISTICS="${P4F}/statistics"

BOOMERANG_SPDS="$(findLocalDir "SPDS-experiments/WPDS" "Boomerang SPDS")"
BOOMERANG_SPDS_EXAMPLE_PATH="$BOOMERANG_SPDS/boomerangPDS/src/main/java/boomerang/example"
BOOMERANG_SPDS_EXAMPLE_PACKAGE="boomerang.example"

BOOMERANG_ORIGINAL="$(findLocalDir "boomerang-artifact" "Boomerang Original")"
BOOMERANG_ORIGINAL_META_PATH="$BOOMERANG_ORIGINAL/Boomerang/example/example"
BOOMERANG_ORIGINAL_OBJECT_PATH="$BOOMERANG_ORIGINAL/Boomerang/exampleTarget/example"
BOOMERANG_ORIGINAL_META_PACKAGE="example"
BOOMERANG_ORIGINAL_OBJECT_PACKAGE="example"

function configure_result {
  RESULT_FILE="$RESULTS_PATH/experiment=$EXPERIMENT--case=$CASE--analysis=$ANALYSIS"
  topk=y
  [ "${ANALYSIS: -6}" == "splume" ] && topk=n
  [ "${ANALYSIS:0:9}" == "boomerang" ] && topk=n
  [ "$topk" == "y" ] && RESULT_FILE="$RESULT_FILE--k=$K"
  RESULT_FILE="$RESULT_FILE--$(date --iso-8601=seconds).txt"
  uptime &>> "$RESULT_FILE"
}

rm -rf $HERE/results
mkdir $HERE/results

lscpu
cat /proc/cpuinfo
free -m
cat /proc/meminfo
uname -a
lsb_release -a
(cd "${ODEFA}" && git rev-parse HEAD)
ocaml -version
opam --version
racket --version
(cd "${P4F}" && git rev-parse HEAD)
java -version
sbt sbtVersion
scala -version

# Utility functions

function streamRewriteJavaResource {
    sed -r 's/^package.*$/package '$1';/g' |\
    sed -r 's/import .*\.TestUtils\.\*;/import '$1'.TestUtils.*;/g'
}

# Analysis preparation functions

function ready_odefa {
    (cd "${ODEFA}" && make)
}

function ready_p4f {
    (cd "${P4F}" && sbt compile)
}

function ready_boomerang_spds {
    ls -1 ${CASES_PATH}/*.java | while read line; do
        cat "$line" | streamRewriteJavaResource "${BOOMERANG_SPDS_EXAMPLE_PACKAGE}" > "${BOOMERANG_SPDS_EXAMPLE_PATH}/$(basename "$line")"
    done
    for supportClass in BoomerangSPDSBenchmarkMain TestUtils; do
        cat "${RESOURCES_PATH}/$supportClass.java" | streamRewriteJavaResource "${BOOMERANG_SPDS_EXAMPLE_PACKAGE}" > "${BOOMERANG_SPDS_EXAMPLE_PATH}/$supportClass.java"
    done
    (cd "${BOOMERANG_SPDS}" && mvn compile)
}

function ready_boomerang_original {
    ls -1 ${CASES_PATH}/*.java | while read line; do
        cat "$line" | streamRewriteJavaResource "${BOOMERANG_ORIGINAL_OBJECT_PACKAGE}" > "${BOOMERANG_ORIGINAL_OBJECT_PATH}/$(basename "$line")"
    done
    dest="${BOOMERANG_ORIGINAL_META_PATH}"
    for supportClass in BoomerangOriginalBenchmarkMain . TestUtils; do
        if [ $supportClass == "." ]; then
            dest="${BOOMERANG_ORIGINAL_OBJECT_PATH}"
            continue
        fi
        cat "${RESOURCES_PATH}/$supportClass.java" | streamRewriteJavaResource "${BOOMERANG_ORIGINAL_META_PACKAGE}" > "$dest/$supportClass.java"
    done;
    (cd "${BOOMERANG_ORIGINAL}/Boomerang" && mkdir -p bin && javac -cp "$(ls -1 lib | while read line; do echo -n "lib/$line:"; done):../builds/boomerang.jar" -d bin $(find example -name '*.java') $(find exampleTarget -name '*.java'))
}

# Analysis execution functions

function ddpa {
  ANALYSIS=ddpa
  configure_result
  cat "${NATODEFA_SOURCE}" | \
    tr '*/%' '+' | \
    "${NATODEFA_TRANSLATOR}" -p -a |\
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${ODEFA_TOPLOOP}" -S"${K}"ddpa --analyze-variables=all --report-sizes --report-analysis-time --report-source-statistics --disable-evaluation --disable-inconsistency-check \
    &>> "${RESULT_FILE}" || true
}

function kplume {
  ANALYSIS=kplume
  configure_result
  cat "${NATODEFA_SOURCE}" | \
    tr '*/%' '+' | \
    "${NATODEFA_TRANSLATOR}" -p -a |\
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${ODEFA_TOPLOOP}" -S"${K}"plume --analyze-variables=all --report-sizes --report-analysis-time --report-source-statistics --disable-evaluation --disable-inconsistency-check \
    &>> "${RESULT_FILE}" || true
}

function splume {
  ANALYSIS=splume
  configure_result
  cat "${NATODEFA_SOURCE}" | \
    tr '*/%' '+' | \
    "${NATODEFA_TRANSLATOR}" -p -a |\
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${ODEFA_TOPLOOP}" -Ssplume --analyze-variables=all --report-sizes --report-analysis-time --report-source-statistics --disable-evaluation --disable-inconsistency-check \
    &>> "${RESULT_FILE}" || true
}

function p4f {
  ANALYSIS=p4f
  configure_result
  rm -rf "${P4F_STATISTICS}"
  if (cd "${P4F}" && /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" scala -J-Xmx16g -J-Xss256m -cp "${P4F_CLASSPATH}" org.ucombinator.cfa.RunCFA --kcfa --k "${K}" --kalloc p4f --dump-statistics "${SCHEME_SOURCE}" &>> "${RESULT_FILE}")
  then
    cat "${P4F_STATISTICS}/"*/*".txt" &>> "${RESULT_FILE}"
  else
    # P4F doesn't get killed properly by timeout
    pids=$(ps ax | grep java | grep RunCFA | egrep -o '^ *[0-9]+')
    if [ -n "$pids" ]; then kill -9 $pids; fi
  fi
}

function boomerangSPDS {
  ANALYSIS=boomerangSPDS
  configure_result
  (
    cd "${BOOMERANG_SPDS}"
    classname="$(basename ${JAVA_SOURCE})"
    classname="${classname//.java/}"
    MAVEN_OPTS="-Xss256m -Xmx16g" /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" mvn exec:java -pl boomerangPDS -Dexec.mainClass=$BOOMERANG_SPDS_EXAMPLE_PACKAGE.BoomerangSPDSBenchmarkMain -Dexec.arguments="${BOOMERANG_SPDS_EXAMPLE_PACKAGE}.${classname}"
  ) &>> "${RESULT_FILE}" || true
}

function boomerangOriginal {
  ANALYSIS="boomerangOriginal"
  configure_result
  (
    cd "${BOOMERANG_ORIGINAL}/Boomerang"
    classname="$(basename ${JAVA_SOURCE})"
    classname="${classname//.java/}"
    vars="$(cat ${JAVA_SOURCE} | egrep -o 'queryFor\([a-zA-Z0-9_]+\);' | egrep -o '\([a-zA-Z0-9_]+\)' | tr -d '()')"
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" java -Xss256m -Xmx16g -cp "$(ls -1 lib | while read line; do echo -n "lib/$line:"; done):../builds/boomerang.jar:bin" "${BOOMERANG_ORIGINAL_META_PACKAGE}.BoomerangOriginalBenchmarkMain" "${BOOMERANG_ORIGINAL_OBJECT_PACKAGE}.${classname}" $vars
  ) &>> "${RESULT_FILE}" || true
}

mkdir -p "${RESULTS_PATH}"

# Set up analysis artifacts
ready_odefa
ready_p4f
ready_boomerang_spds
ready_boomerang_original

# Run benchmarks
for TRIAL in $(seq 1 "${TRIALS}")
do
  echo "######## Trial $TRIAL/$TRIALS"
  for CASE in "${!CASES[@]}"
  do
    echo "##### Case $CASE"
    SCHEME_SOURCE="${CASES_PATH}/${CASE}.scm"
    NATODEFA_SOURCE="${CASES_PATH}/${CASE}.natodefa"
    JAVA_SOURCE="${CASES_PATH}/$(echo "${CASE^}" | tr '-' '_').java"
    EXPERIMENT=polyvariant
    K="${CASES[${CASE}]}"
    ddpa
    kplume
    K=1
    p4f
    splume
    boomerangSPDS
    #boomerangOriginal
  done
done

echo "END"
