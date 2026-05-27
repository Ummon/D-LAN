# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib network
QT -= gui
TARGET = TestsFileManager
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../../Libs/protobuf.pri)
include(../../../Common/common.pri)

QMAKE_CXXFLAGS_WARN_ON += -Wno-pessimizing-move -Wno-unused-result

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
    CacheTest.cpp \
    MockHashCache.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    HashesReceiver.cpp \
    StressTest.cpp \
    ../../../Protos/core_settings.pb.cc \
    StressTests.cpp \
    Utils.cpp \
    WordIndexTests.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    CacheTest.h \
    HashesReceiver.h \
    MockHashCache.h \
    StressTest.h \
    ../../../Protos/core_settings.pb.h \
    StressTests.h \
    Utils.h \
    WordIndexTests.h
