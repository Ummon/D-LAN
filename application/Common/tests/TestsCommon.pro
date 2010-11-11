# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests

CONFIG += link_prl

include(../../Common/common.pri)

LIBS += -L"../output/debug" -lCommon
POST_TARGETDEPS += ../output/debug/libCommon.a

LIBS += -L${PROTOBUF}/src/.libs \
   -lprotobuf

INCLUDEPATH += . \
   .. \
   ../.. \
   ${PROTOBUF}/src

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../Protos/common.pb.cc
HEADERS += Tests.h
