#!/usr/bin/env bash

cloc --exclude-dir=Protos,Libs,.tmp,output --exclude-lang=make,IDL ..
