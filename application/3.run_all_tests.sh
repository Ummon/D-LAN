#!/usr/bin/env bash

# Runs all tests. They must exist.


if [ `uname -s` = "Linux" ] ; then
	EXTENSION=
else
	EXTENSION=.exe
fi


# Common.
cd Common/tests/output/release
./Tests$EXTENSION
ERR=$?
cd ../../../..
if (( $ERR )); then exit 1; fi


# FileManager.
cd Core/FileManager/tests/output/release
./Tests$EXTENSION
ERR=$?
cd ../../../../..
if (( $ERR )); then exit 1; fi


# PeerManager.
cd Core/PeerManager/tests/output/release
./Tests$EXTENSION
ERR=$?
cd ../../../../..
if (( $ERR )); then exit 1; fi

