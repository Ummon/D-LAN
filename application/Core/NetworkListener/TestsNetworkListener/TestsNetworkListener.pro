QT += testlib
QT += network
QT -= gui
TARGET = Tests
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

CONFIG += link_prl console
CONFIG -= app_bundle

include(../../../Libs/protobuf.pri)
include(../../../Libs/blake3.pri)
include(../../../Common/common.pri)

QMAKE_CXXFLAGS_WARN_ON += -Wno-pessimizing-move -Wno-unused-result

LIBS += -L../../FileManager/output/$$FOLDER -lFileManager
POST_TARGETDEPS += ../../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../NetworkListener/output/$$FOLDER -lNetworkListener
POST_TARGETDEPS += ../../NetworkListener/output/$$FOLDER/libNetworkListener.a

LIBS += -L../../PeerManager/output/$$FOLDER -lPeerManager
POST_TARGETDEPS += ../../PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -L../../../Common/LogManager/output/$$FOLDER -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../../../Common/output/$$FOLDER -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

INCLUDEPATH += . \
    .. \ # NetworkListener
    ../.. \ # Core
    ../../.. \ # For access to Common and Protos
    ${PROTOBUF}/src


# win32 {
   # INCLUDEPATH += "."
   # INCLUDEPATH += "$$(QTDIR)\..\mingw\include"
   # LIBS += "$$(QTDIR)\..\mingw\lib\libwsock32.a"
# }

TEMPLATE = app

SOURCES += main.cpp \
    Tests.cpp
HEADERS += Tests.h
