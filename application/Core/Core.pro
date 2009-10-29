# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:20:26
# -------------------------------------------------
QT -= gui
QT += network
TARGET = Core
CONFIG += link_prl
INCLUDEPATH += . \
    .. \
    ${PROTOBUF}/src

LIBS += -LFileManager/output/debug \
    -lFileManager
LIBS += -LNetworkListener/output/debug \
    -lNetworkListener
LIBS += -LPeerManager/output/debug \
    -lPeerManager
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
LIBS += -L../Common/LogManager/output/debug \
    -lLogManager
LIBS += -L../Common/output/debug \
    -lCommon

# FIXME : Theses declarations should not be here, all dependencies are read from the prl files of each library (see link_prl):
win32 {
    INCLUDEPATH += "."
    INCLUDEPATH += "$$(QTDIR)\..\mingw\include"
    LIBS += "$$(QTDIR)\..\mingw\lib\libwsock32.a"
}

DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Core.cpp
HEADERS += Core.h
