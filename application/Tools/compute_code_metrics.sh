#!/usr/bin/env bash

cloc --exclude-dir=Protos,Libs,.tmp --exclude-lang=make,IDL ..
