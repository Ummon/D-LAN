QT += testlib
QT += network
QT -= gui
TARGET = Tests
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

CONFIG += link_prl

INCLUDEPATH += . \
    .. \ # NetworkListener
    ../.. \ # Core
    ../../.. \ # For access to Common and Protos
    ${PROTOBUF}/src

LIBS += -L../../FileManager/output/debug \
   -lFileManager

LIBS += -L../../NetworkListener/output/debug \
   -lNetworkListener

LIBS += -L../../PeerManager/output/debug \
   -lPeerManager

LIBS += -L../../../Common/LogManager/output/debug \
   -lLogManager

LIBS += -L../../../Common/output/debug \
   -lCommon

# FIXME : Theses declarations should not be here, all dependencies are read from the prl files of each library (see link_prl):
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
win32 {
    INCLUDEPATH += "."
    INCLUDEPATH += "$$(QTDIR)\..\mingw\include"
    LIBS += "$$(QTDIR)\..\mingw\lib\libwsock32.a"
}

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp
HEADERS += Tests.h
