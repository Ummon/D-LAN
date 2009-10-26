# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests

debug {
   DESTDIR = "output/debug"
   MOC_DIR = ".tmp/debug"
   OBJECTS_DIR = ".tmp/debug"
   LIBS += -L../output/debug \
       -lFileManager
   LIBS += -L../../../Common/output/debug \
       -lCommon
} else {
   DESTDIR = "output/release"
   MOC_DIR = ".tmp/release"
   OBJECTS_DIR = ".tmp/release"
   LIBS += -L../output/release \
       -lFileManager
   LIBS += -L../../../Common/output/release \
       -lCommon
}

LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    .. \
    ../../.. \ # For the 'Common' component.
    ${PROTOBUF}/src
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc
HEADERS += Tests.h \
    ../../../Protos/common.pb.h
