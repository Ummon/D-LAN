#!/usr/bin/env bash
# Generates makefiles and compile all components and theirs tests.

SPEC=win32-g++
MAKE=mingw32-make.exe

cd Tools
./update_version.sh
cd ..

# To force to recompile the Common/Version.rs.
rm Core/.tmp/release/version_res.o
rm GUI/.tmp/release/version_res.o

# Common.
cd Common
qmake Common.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ..

# LogManager.
cd Common/LogManager
qmake LogManager.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# LogViewer.
cd Tools/LogViewer
qmake LogViewer.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# FileManager.
cd Core/FileManager
qmake FileManager.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# PeerManager.
cd Core/PeerManager
qmake PeerManager.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# UploadManager.
cd Core/UploadManager
qmake UploadManager.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# DownloadManager.
cd Core/DownloadManager
qmake DownloadManager.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# NetworkListener.
cd Core/NetworkListener
qmake NetworkListener.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

# RemoteControlManager.
cd Core/RemoteControlManager
qmake RemoteControlManager.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ../..

#Core
cd Core
qmake Core.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ..

# GUI
cd GUI
qmake GUI.pro -r -spec $SPEC CONFIG+=release
$MAKE -w
cd ..


