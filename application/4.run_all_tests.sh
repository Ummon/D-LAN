#!/usr/bin/env bash
# Runs all tests. They must exist.

set -o errexit

if [ `uname -s` = "Linux" ] ; then
   EXTENSION=
else
   EXTENSION=.exe
fi

TESTS=(
   Common/TestsCommon/output/release/TestsCommon$EXTENSION
   Core/FileManager/TestsFileManager/output/release/TestsFileManager$EXTENSION
   Core/PeerManager/TestsPeerManager/output/release/TestsPeerManager$EXTENSION
   # Core/DownloadManager/TestsDownloadManager/output/release/TestsDownloadManager$EXTENSION
)

for i in ${TESTS[@]}
do
   pushd .
   cd `dirname ${i}`
   TEST=`echo ${i} | awk -F"/" '{print $NF}'`
   echo "Executing $TEST.."
   ./$TEST
   popd
done

echo "All tests finished successfully"
