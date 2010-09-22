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
    -lPeerManager
POST_TARGETDEPS += ../output/$$FOLDER/libPeerManager.a

LIBS += -L../../FileManager/output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += ../../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

# FIXME : Should not be here, all dependies are read from the prl file (see link_prl):
LIBS += -L../../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    .. \
    ../../.. \ # For the 'Common' component.
    ${PROTOBUF}/src
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    TestServer.cpp \
    PeerUpdater.cpp \
    ResultListener.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    debug_new.h \
    TestServer.h \
    PeerUpdater.h \
    Constants.h \
    ResultListener.h
