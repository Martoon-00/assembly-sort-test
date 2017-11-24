#!/usr/bin/env bash
set -e
set -o pipefail

# include tests
tests=false
# include benchmarks
benchs=false
# run tests/benchmarks, not only build
run=false
# stop after first fail in tests
fail_fast=false

### Gather passed arguments

for var in "$@"
do
    if [[ $var == "test" ]]; then
        tests=true
    elif [[ $var == "bench" ]]; then
        benchs=true
    elif [[ $var == "run" ]]; then
        run=true
    fi
done

### Form command

cmd="stack build"

if [[ $tests == true ]]; then
   cmd="$cmd --test"
fi
if [[ $benchs == true ]]; then
   cmd="$cmd --bench"
fi
if [[ $run == false ]]; then
    cmd="$cmd --no-run-tests --no-run-benchmarks"
fi

echo "Executing: $cmd"
$cmd

