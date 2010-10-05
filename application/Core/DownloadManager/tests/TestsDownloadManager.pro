#-------------------------------------------------
# Project created by QtCreator 2010-09-15T02:48:03
#-------------------------------------------------
QT += testlib network
QT -= gui
TARGET = Tests
CONFIG += link_prl \
    console
CONFIG -= app_bundle
CONFIG(debug, debug|release) {
    DEFINES += DEBUG
    FOLDER = debug
}
else:FOLDER = release
DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER

LIBS += -L../output/$$FOLDER \
    -lDownloadManager
POST_TARGETDEPS += ../output/$$FOLDER/libDownloadManager.a

LIBS += -L../../UploadManager/output/$$FOLDER \
    -lUploadManager
POST_TARGETDEPS += ../../UploadManager/output/$$FOLDER/libUploadManager.a

LIBS += -L../../PeerManager/output/$$FOLDER \
    -lPeerManager
POST_TARGETDEPS += ../../PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -L../../FileManager/output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += ../../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

# FIXME : Should not be here, all dependencies are read from the prl file (see link_prl):
LIBS += -L../../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    .. \
    ../../PeerManager \ # Because Core/PeerManager/tests/PeerUpdater needs to include Core/PeerManager/priv/PeerManager.h.
    ../../.. \ # For the 'Common' component.
    ${PROTOBUF}/src

TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    ../../PeerManager/tests/TestServer.cpp \
    ../../PeerManager/tests/PeerUpdater.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    ../../PeerManager/tests/TestServer.h \
    ../../PeerManager/tests/PeerUpdater.h
