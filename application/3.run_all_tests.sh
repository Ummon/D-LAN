#!/usr/bin/env bash
# Runs all tests. They must exist.

set -o errexit

TESTS=(
   Common/TestsCommon/output/release/TestsCommon.exe
   Core/FileManager/TestsFileManager/output/release/TestsFileManager.exe
   Core/PeerManager/TestsPeerManager/output/release/TestsPeerManager.exe
   # Core/DownloadManager/TestsDownloadManager/output/release/TestsDownloadManager.exe
)

for i in ${TESTS[@]}
do
   pushd .
   cd `dirname ${i}`
   TEST=`echo ${i} | awk -F"/" '{print $NF}'`
   echo "Executing $TEST.."
   $TEST
   popd
done

Echo "All tests finished successfully"
