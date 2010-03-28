# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:20:26
# -------------------------------------------------
QT -= gui
QT += network
TARGET = Core
CONFIG += link_prl


CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

INCLUDEPATH += . \
    .. \
    ${PROTOBUF}/src

LIBS += -LFileManager/output/$$FOLDER \
    -lFileManager
LIBS += -LNetworkListener/output/$$FOLDER \
    -lNetworkListener
LIBS += -LPeerManager/output/$$FOLDER \
    -lPeerManager
LIBS += -L../Common/LogManager/output/$$FOLDER \
    -lLogManager
LIBS += -L../Common/output/$$FOLDER \
    -lCommon
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf

# FIXME : Theses declarations should not be here, all dependencies are read from the prl files of each library (see link_prl):
win32 {
    INCLUDEPATH += "."
    INCLUDEPATH += "$$(QTDIR)\..\mingw\include"
    LIBS += "$$(QTDIR)\..\mingw\lib\libwsock32.a"
}

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Core.cpp
HEADERS += Core.h
