#!/usr/bin/env bash

set -o errexit

./1.generate_protos_cpp.sh
./2.compile_all_components.sh --clean
./3.run_all_tests.sh
./4.build_installation_package.sh
