QT += testlib network
QT -= gui
TARGET = TestsDownloadManager
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../../Common/common.pri)
include(../../../Libs/protobuf.pri)
include(../../../Protos/Protos.pri)


LIBS += -L../output/$$FOLDER \
    -lDownloadManager
POST_TARGETDEPS += ../output/$$FOLDER/libDownloadManager.a

LIBS += -L../../FileManager/output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += ../../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../PeerManager/output/$$FOLDER \
    -lPeerManager
POST_TARGETDEPS += ../../PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -L../../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

# FIXME: Should not be here, all dependencies are read from the prl file (see link_prl):
LIBS += -L../../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a

INCLUDEPATH += . \
    .. \
    ../.. \
    ../../.. # For the 'Common' component.
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    ../../../Protos/core_settings.pb.cc \
    ../../../Protos/core_protocol.pb.cc \ 
    MockFileManager.cpp \
    MockPeerManager.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    ../../../Protos/core_settings.pb.h \
    ../../../Protos/core_protocol.pb.h \
    MockFileManager.h \
    MockPeerManager.h
