#!/usr/bin/env bash

./1.generate_protos_cpp.sh && ./2.compile_all_components.sh && ./3.run_all_tests.sh && ./4.build_installation_package.sh