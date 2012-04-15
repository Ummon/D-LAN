# -------------------------------------------------
# Project created by QtCreator 2009-10-05T19:14:23
# -------------------------------------------------
QT -= gui
QT += network
TARGET = PeerManager
TEMPLATE = lib

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

CONFIG += staticlib link_prl create_prl
INCLUDEPATH += . ../..

DEFINES += PEERMANAGER_LIBRARY
SOURCES += priv/PeerManager.cpp \
    priv/Peer.cpp \
    priv/Builder.cpp \
    priv/ConnectionPool.cpp \
    priv/GetEntriesResult.cpp \
    priv/GetHashesResult.cpp \
    priv/GetChunkResult.cpp \
    priv/Log.cpp \
    priv/PeerSelf.cpp \
    priv/PeerMessageSocket.cpp
HEADERS += IPeerManager.h \
    IPeer.h \
    priv/PeerManager.h \
    priv/Peer.h \
    Builder.h \
    priv/Log.h \
    priv/Constants.h \
    priv/ConnectionPool.h \
    priv/PeerMessageSocket.h \
    IGetEntriesResult.h \
    IGetHashesResult.h \
    IGetChunkResult.h \
    ISocket.h \
    priv/GetEntriesResult.h \
    priv/GetHashesResult.h \
    priv/GetChunkResult.h \
    priv/PeerSelf.h
