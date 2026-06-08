#-------------------------------------------------
# Project created by QtCreator 2010-09-15T02:48:03
#-------------------------------------------------
QT += testlib network
QT -= gui
TARGET = TestsPeerManager
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../../Libs/protobuf.pri)
include(../../../Libs/blake3.pri)
include(../../../Common/common.pri)

LIBS += -L../output/$$FOLDER -lPeerManager
POST_TARGETDEPS += ../output/$$FOLDER/libPeerManager.a

LIBS += -L../../FileManager/output/$$FOLDER -lFileManager
POST_TARGETDEPS += ../../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../../Common/output/$$FOLDER -lCommon
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
    MockHashCache.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    ../../../Protos/core_settings.pb.cc \
    ../../../Protos/core_protocol.pb.cc \
    TestServer.cpp \
    PeerUpdater.cpp \
    ResultListener.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    ../../../Protos/core_settings.pb.h \
    ../../../Protos/core_protocol.pb.h \
   MockHashCache.h \
    TestServer.h \
    PeerUpdater.h \
    ResultListener.h \
