# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests

CONFIG += link_prl

DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

LIBS += -L"../output/debug" -lCommon
POST_TARGETDEPS += ../output/debug/libCommon.a

LIBS += -L${PROTOBUF}/src/.libs -lprotobuf

INCLUDEPATH += . \
   ..\
   ${PROTOBUF}/src

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp
HEADERS += Tests.h
