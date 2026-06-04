# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib network
QT -= gui
TARGET = TestsCommon
CONFIG += link_prl console
CONFIG -= app_bundle

include(../common.pri)
include(../../Libs/blake3.pri)
include(../../Libs/protobuf.pri)

QMAKE_CXXFLAGS_WARN_ON += -Wno-pessimizing-move -Wno-unused-result

LIBS += -L"../output/$$FOLDER" -lCommon
POST_TARGETDEPS += ../output/$$FOLDER/libCommon.a

LIBS += -L../LogManager/output/$$FOLDER -lLogManager
POST_TARGETDEPS += ../LogManager/output/$$FOLDER/libLogManager.a

INCLUDEPATH += . \
   .. \
   ../..

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../Protos/common.pb.cc \
    ../../Protos/core_settings.pb.cc \
    TreeTests.cpp \
    BenchmarkTests.cpp
HEADERS += Tests.h \
    TreeTests.h \
    BenchmarkTests.h
