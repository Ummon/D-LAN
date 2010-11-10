# -------------------------------------------------
# Project created by QtCreator 2009-10-05T20:02:39
# -------------------------------------------------
QT += network
QT -= gui
TARGET = NetworkListener
TEMPLATE = lib

CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

include(../../Libs/protobuf.pri)

CONFIG += staticlib create_prl link_prl

INCLUDEPATH += . \
    ../..

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER

LIBS += -L../DownloadManager/output/$$FOLDER -lDownloadManager
POST_TARGETDEPS += ../DownloadManager/output/$$FOLDER/libDownloadManager.a

LIBS += -L../PeerManager/output/$$FOLDER -lPeerManager
POST_TARGETDEPS += ../PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -L../FileManager/output/$$FOLDER -lFileManager
POST_TARGETDEPS += ../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../../Common/LogManager/output/$$FOLDER -lLogManager
POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../../Common/output/$$FOLDER -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

DEFINES += NETWORKLISTENER_LIBRARY
SOURCES += priv/UDPListener.cpp \
    priv/TCPListener.cpp \
    priv/Search.cpp \
    priv/NetworkListener.cpp \
    priv/ChunkUpdater.cpp \
    priv/Chat.cpp \
    priv/Builder.cpp \
    ../../Protos/common.pb.cc \
    ../../Protos/core_protocol.pb.cc
HEADERS += ISearch.h \
    INetworkListener.h \
    IChat.h \
    priv/UDPListener.h \
    priv/TCPListener.h \
    priv/Search.h \
    priv/NetworkListener.h \
    priv/ChunkUpdater.h \
    priv/Chat.h \
    Builder.h \
    ../../Protos/common.pb.h \
    ../../Protos/core_protocol.pb.h \
    priv/Log.h
