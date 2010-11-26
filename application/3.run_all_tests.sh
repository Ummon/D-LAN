#!/usr/bin/env bash

# Runs all tests. They must exist.

# Common.
cd Common/tests/output/release
./Tests.exe
ERR=$?
cd ../../../..
if (( $ERR )); then exit 1; fi