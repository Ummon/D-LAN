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

CONFIG += staticlib link_prl create_prl
INCLUDEPATH += . ../.. ${PROTOBUF}/src

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER

DEFINES += PEERMANAGER_LIBRARY
SOURCES += priv/PeerManager.cpp \
    priv/Peer.cpp \
    priv/Builder.cpp \
    priv/ConnectionPool.cpp \
    priv/Socket.cpp \
    priv/GetEntriesResult.cpp \
    priv/GetHashesResult.cpp \
    priv/GetChunkResult.cpp
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
    IGetChunkResult.h \
    ISocket.h \
    priv/GetEntriesResult.h \
    priv/GetHashesResult.h \
    priv/GetChunkResult.h
