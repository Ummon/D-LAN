#!/usr/bin/env bash

# This script will integrate the license '../license.txt' to all source (*.h, *.cpp)
# If the license already exists in a source it will be replaced.

# TODO

cd ..

for i in *.cpp
do
   echo $i
  #if ! grep -q Copyright $i
  #then
  #  cat copyright.txt $i >$i.new && mv $i.new $i
  #fi
done


