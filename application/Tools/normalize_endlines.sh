#!/usr/bin/env bash

set -o errexit

cd ..

find . -not -path "./Libs*" -and -not -path "*/.tmp*" -and -not -path "./Protos*" -and \( -name '*.ui' -or -name '*.pro' -or -name '*.cpp' -or -name '*.h' \) -exec dos2unix -U {} \;
