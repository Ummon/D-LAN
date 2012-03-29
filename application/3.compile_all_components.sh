#!/usr/bin/env bash
# Generates makefiles and compile all components and theirs tests.

set -o errexit

PROJECTS=(
   Common
   Common/TestsCommon
   Common/LogManager
   Common/RemoteCoreController
   Core/FileManager
   Core/FileManager/TestsFileManager
   Core/PeerManager
   Core/PeerManager/TestsPeerManager
   Core/UploadManager
   Core/DownloadManager
   Core/NetworkListener
   Core/RemoteControlManager
   Core
   GUI
   Tools/LogViewer
   Tools/PasswordHasher
)
if [ `uname -s` = "Linux" ] ; then
   SPEC=linux-g++
   MAKE=make
elif [ `uname -s` = "Darwin" ] ; then # Mac OS X.
   SPEC=macx-g++
   MAKE=make
else
   SPEC=win32-g++
   MAKE=mingw32-make.exe
fi
CLEAN_COMMAND=off

# Read the script arguments.
for arg in $@
do
   if [ $arg == "--prof" ]
   then
      PROF=prof
      echo "Profling activated"
   elif [ $arg == "--clean" ]
   then
      CLEAN_COMMAND=on
      echo "Clean activated"
   elif [ $arg == "-h" ] || [ $arg == "--help" ]
   then
      echo "Usage : $0 [--prof] [--clean]"
      echo " --prof : To active profiling"
      echo " --clean : Clean temporary objects before each compilation"
      exit
   fi
done

cd Tools
./update_version.sh
cd ..

# To force to recompile the Common/Version.rs and DialogAbout.
rm -f Core/.tmp/release/version_res.o
rm -f GUI/.tmp/release/version_res.o
rm -f GUI/.tmp/release/DialogAbout.o

for i in ${PROJECTS[@]}
do
   pushd .
   cd ${i}
   PROJECT_NAME=`echo ${i} | awk -F"/" '{print $NF}'`
   echo Compiling $PROJECT_NAME..
   qmake ${PROJECT_NAME}.pro -r -spec $SPEC "CONFIG+=release $PROF"
   if [ $CLEAN_COMMAND == on ]
   then
      $MAKE release-clean -w || { echo "nothing to clean"; } # To avoid the command to fail.
   fi
   $MAKE -w
   popd
done

echo "Compilation finished successfully"
