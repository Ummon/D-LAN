# -------------------------------------------------
# Project created by QtCreator 2009-10-05T19:14:23
# -------------------------------------------------
QT -= gui
QT += network
TARGET = PeerManager
TEMPLATE = lib

CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

CONFIG += staticlib \
    link_prl \
    create_prl
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    ../.. \
    ${PROTOBUF}/src

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER

LIBS += -L../FileManager/output/$$FOLDER \
     -lFileManager
POST_TARGETDEPS += ../FileManager/output/$$FOLDER/libFileManager.a
LIBS += -L../../Common/LogManager/output/$$FOLDER \
     -lLogManager
POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a
LIBS += -L../../Common/output/$$FOLDER \
     -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a
DEFINES += PEERMANAGER_LIBRARY
SOURCES += priv/PeerManager.cpp \
    priv/Peer.cpp \
    priv/Builder.cpp \
    priv/ConnectionPool.cpp \
    priv/Socket.cpp
HEADERS += IPeerManager.h \
    IPeer.h \
    priv/PeerManager.h \
    priv/Peer.h \
    Builder.h \
    priv/Log.h \
    priv/Constants.h \
    priv/ConnectionPool.h \
    priv/Socket.h \
    IGetEntriesResult.h \
    IGetHashesResult.h \
    IGetChunkResult.h
