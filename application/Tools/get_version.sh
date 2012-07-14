#!/usr/bin/env bash

VERSION_FILE=../Common/Version.h

VERSION=`sed -n  's/#define VERSION "\(.*\)"/\1/p' $VERSION_FILE`
VERSION_TAG=`sed -n  's/#define VERSION_TAG "\(.*\)"/\1/p' $VERSION_FILE`

echo $VERSION$VERSION_TAG