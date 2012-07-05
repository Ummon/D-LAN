#!/bin/bash

if (( $# < 1 ))
then
   echo "Usage : $0 <target>"
   echo " <target> : a directory where the website will be deployed"
   exit 1
fi;

TARGET=$1
mkdir $TARGET

if [ ! -d $TARGET ]
then
   echo "The target directory \"$TARGET\" doesn't exist"
   exit 1
fi;

RSYNC_OPTS=-av

cd ..

mkdir $TARGET/db # Yaws should have the permission to write in this folder.

rsync $RSYNC_OPTS colorbox $TARGET
rsync $RSYNC_OPTS files $TARGET --exclude '*.*'
rsync $RSYNC_OPTS img $TARGET
rsync $RSYNC_OPTS modules/ebin $TARGET/modules/
rsync $RSYNC_OPTS modules/include $TARGET/modules/
rsync $RSYNC_OPTS yssi $TARGET
rsync $RSYNC_OPTS js $TARGET
rsync $RSYNC_OPTS favicon.ico $TARGET
rsync $RSYNC_OPTS index.yaws $TARGET
rsync $RSYNC_OPTS style.css $TARGET
 
