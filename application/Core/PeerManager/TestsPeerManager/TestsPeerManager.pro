#-------------------------------------------------
# Project created by QtCreator 2010-09-15T02:48:03
#-------------------------------------------------
QT += testlib network
QT -= gui
TARGET = TestsPeerManager
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../../Libs/protobuf.pri)
include(../../../Common/common.pri)

LIBS += -L../output/$$FOLDER \
    -lPeerManager
POST_TARGETDEPS += ../output/$$FOLDER/libPeerManager.a

LIBS += -L../../FileManager/output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += ../../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

# FIXME: Should not be here, all dependencies are read from the prl file (see link_prl):
LIBS += -L../../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a

INCLUDEPATH += . \
    .. \
    ../../.. # For the 'Common' component.
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    TestServer.cpp \
    PeerUpdater.cpp \
    ResultListener.cpp \
    ../../../Protos/core_settings.pb.cc
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    TestServer.h \
    PeerUpdater.h \
    ResultListener.h \
    ../../../Protos/core_settings.pb.h
