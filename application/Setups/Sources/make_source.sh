#!/usr/bin/env bash

cd ../../Tools

CURRENT_VERSION=`get_version.sh`
CURRENT_DATE=`date -u +%Y-%m-%d_%H-%M`

cd ../..

INST_DIR=application/Installations

if [ ! -d "$INST_DIR" ]; then
   mkdir "$INST_DIR"
fi

CURRENT_TREE=`git show --pretty="%H" HEAD | head -n 1`

git archive --format=tar.gz $CURRENT_TREE > "$INST_DIR/D-LAN-${CURRENT_VERSION}-${CURRENT_DATE}-${CURRENT_TREE}.tar.gz"