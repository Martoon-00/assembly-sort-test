# assembly-sort-test

[![Build Status](https://travis-ci.org/Martoon-00/assembly-sort-test.svg?branch=master)](https://travis-ci.org/Martoon-00/assembly-sort-test)

Provides bunch of tests for assembly task which as part of Professional Development Program.

## How to use

All parameters specific to your program are passed via environment, for instance:

`PROG_PATH='../assembly-task/sort' INPUT_FILE='sort.in' stack test`.

Also some benchmarks are present, however I'm not sure they produce reasonable measurements :/

## Requirements

Among mentioned ones, here are requirements which tests expect to hold:

* Found values should go to `stdout`, any errors should go to `stderr`, including "Not found" error.

All checks for error scenarious can be disabled via defining `LAX_TESTS` in env though.

* Program should crash with nonzero exit code if input file processing failed.
Behavior for "Not found" error is not fixated yet.

`LAX_TESTS=1` to disable.

* Program should be able to accept multiple queries until end of input.

`ONE_QUERY=1` to disable.

* Program should work with any common way of representing newlines, i.e. `LF`, `CR`, `CR+LF`.

`LF_ONLY` to disable.

## Notes

* Overwhelmed with quickcheck output? Keep `--test-arguments --fail-fast` in mind!

* Your contribution is very welcome!
