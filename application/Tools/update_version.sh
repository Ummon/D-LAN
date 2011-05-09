#!/usr/bin/env bash

set -o errexit

# Will update the two fields 'BUILD_TIME' and 'GIT_VERSION' from the file 'application/Common/Version.h'.
CURRENT_DATE=`date -u +%Y-%m-%d_%H-%M`
CURRENT_GIT_VERSION=`git show --pretty="%H" HEAD | head -n 1`
VERSION_FILE=../Common/Version.h

if [ `uname -s` = "Darwin" ] ; then # Mac OS X.
   SED_OPTIONS="-E -n -i backup"
else
   SED_OPTION="-i"
fi

sed $SED_OPTION "s/BUILD_TIME \"[^\"]*\"/BUILD_TIME \"$CURRENT_DATE\"/g" $VERSION_FILE
sed $SED_OPTION "s/GIT_VERSION \"[^\"]*\"/GIT_VERSION \"$CURRENT_GIT_VERSION\"/g" $VERSION_FILE
