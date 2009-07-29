#!/usr/bin/env bash

PROTOS_DIR=Protos

cd $PROTOS_DIR
protoc --cpp_out . *.proto
cd ..

echo cpp and .h generated from $PROTOS_DIR/*.proto