#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

echo "START"

TRIALS=10
TIMEOUT=2h
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
)

TRIALS=5
TIMEOUT=30m
HERE="$(cd "$(dirname $0)" && pwd)"
CASES_PATH="${HERE}/cases"
RESULTS_PATH="${HERE}/results"
ODEFA="${HERE}/.."
ODEFA_TOPLOOP="${ODEFA}/toploop"
NATODEFA_TRANSLATOR="${ODEFA}/translator"
P4F="${ODEFA}/../p4f"
[ ! -d "$P4F" ] && P4F="${ODEFA}/local/p4f"
[ ! -d "$P4F" ] && ( echo "P4F not found!" ; exit 1 )
P4F_CLASSPATH="${P4F}/target/scala-2.11/classes"
P4F_STATISTICS="${P4F}/statistics"

function configure_result {
  RESULT="$RESULTS_PATH/experiment=$EXPERIMENT--case=$CASE--analysis=$ANALYSIS"
  [ "${ANALYSIS: -6}" != "splume" ] && RESULT="$RESULT--k=$K"
  RESULT="$RESULT--$(date --iso-8601=seconds).txt"
  uptime &>> "$RESULT"
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

function ddpa {
  ANALYSIS=ddpa
  configure_result
  cat "${NATODEFA_SOURCE}" | \
    tr '*/%' '+' | \
    "${NATODEFA_TRANSLATOR}" -p |\
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${ODEFA_TOPLOOP}" -S"${K}"ddpa --analyze-variables=all --report-sizes --report-analysis-time --report-source-statistics --disable-evaluation --disable-inconsistency-check \
    &>> "${RESULT}" || true
}

function kplume {
  ANALYSIS=kplume
  configure_result
  cat "${NATODEFA_SOURCE}" | \
    tr '*/%' '+' | \
    "${NATODEFA_TRANSLATOR}" -p |\
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${ODEFA_TOPLOOP}" -S"${K}"plume --analyze-variables=all --report-sizes --report-analysis-time --report-source-statistics --disable-evaluation --disable-inconsistency-check \
    &>> "${RESULT}" || true
}

function splume {
  ANALYSIS=splume
  configure_result
  cat "${NATODEFA_SOURCE}" | \
    tr '*/%' '+' | \
    "${NATODEFA_TRANSLATOR}" -p |\
    /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${ODEFA_TOPLOOP}" -Ssplume --analyze-variables=all --report-sizes --report-analysis-time --report-source-statistics --disable-evaluation --disable-inconsistency-check \
    &>> "${RESULT}" || true
}

function p4f {
  ANALYSIS=p4f
  configure_result
  rm -rf "${P4F_STATISTICS}"
  if (cd "${P4F}" && /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" scala -J-Xmx7g -J-Xss256m -cp "${P4F_CLASSPATH}" org.ucombinator.cfa.RunCFA --kcfa --k "${K}" --kalloc p4f --dump-statistics "${SCHEME_SOURCE}" &>> "${RESULT}")
  then
    cat "${P4F_STATISTICS}/"*/*".txt" &>> "${RESULT}"
  else
    pkill sbt scala java || true
  fi
}

mkdir -p "${RESULTS_PATH}"
(cd "${ODEFA}" && make)
(cd "${P4F}" && sbt compile)

for TRIAL in $(seq 1 "${TRIALS}")
do
  echo "######## Trial $TRIAL"
  for CASE in "${!CASES[@]}"
  do
    echo "##### Case $CASE"
    SCHEME_SOURCE="${CASES_PATH}/${CASE}.scm"
    NATODEFA_SOURCE="${CASES_PATH}/${CASE}.natodefa"
    EXPERIMENT=monovariant
    K=0
    ddpa
    kplume
    p4f
    EXPERIMENT=polyvariant
    K="${CASES[${CASE}]}"
    ddpa
    kplume
    K=1
    p4f
    splume
  done
done

echo "END"
