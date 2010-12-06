# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../../Libs/protobuf.pri)
include(../../../Common/common.pri)

LIBS += -L../output/$$FOLDER -lFileManager
POST_TARGETDEPS += ../output/$$FOLDER/libFileManager.a

LIBS += -L../../../Common/output/$$FOLDER -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

LIBS += -L../../../Common/LogManager/output/$$FOLDER -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a

INCLUDEPATH += . \
    .. \
    ../../.. # For the 'Common' component.
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    HashesReceiver.cpp \
    StressTest.cpp \
    ../../../Protos/core_settings.pb.cc \
    StressTests.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    HashesReceiver.h \
    StressTest.h \
    ../../../Protos/core_settings.pb.h \
    StressTests.h
