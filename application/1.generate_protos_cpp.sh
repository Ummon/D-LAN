#!/usr/bin/env bash
# Generate all .h/.cpp from the proto files.
set -o errexit

PROTOS_DIR=Protos

cd $PROTOS_DIR
protoc --cpp_out . *.proto
cd ..

echo cpp and .h generated from $PROTOS_DIR/*.proto
