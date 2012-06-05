#!/usr/bin/env bash

cd ..

# List all the cpp files sorted by their number of lines.
find . -path "./Libs" -prune -o -path "*/.tmp" -prune -o -name "*.cpp" -exec wc -l {} \; | sort -n -r | less
