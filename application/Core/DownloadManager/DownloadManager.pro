# -------------------------------------------------
# Project created by QtCreator 2009-10-05T18:53:58
# -------------------------------------------------
QT += network
QT -= gui
TARGET = DownloadManager
TEMPLATE = lib

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

CONFIG += staticlib \
    link_prl \
    create_prl

INCLUDEPATH += . \
    ../..

LIBS += -L../../Common/LogManager/output/$$FOLDER \
     -lLogManager
POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a
LIBS += -L../../Common/output/$$FOLDER \
     -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a
LIBS += -L../FileManager/output/$$FOLDER \
     -lFileManager
POST_TARGETDEPS += ../FileManager/output/$$FOLDER/libFileManager.a
LIBS += -L../PeerManager/output/$$FOLDER \
     -lPeerManager
POST_TARGETDEPS += ../PeerManager/output/$$FOLDER/libPeerManager.a

DEFINES += DOWNLOADMANAGER_LIBRARY

SOURCES += priv/FileDownload.cpp \
    priv/DownloadManager.cpp \
    priv/Download.cpp \
    priv/DirDownload.cpp \
    priv/ChunkDownload.cpp \
    ../../Protos/common.pb.cc \
    priv/Builder.cpp \
    priv/OccupiedPeers.cpp \
    ../../Protos/queue.pb.cc
HEADERS += IDownloadManager.h \
    IDownload.h \
    IChunkDownload.h \
    priv/FileDownload.h \
    priv/DownloadManager.h \
    priv/Download.h \
    priv/DirDownload.h \
    priv/ChunkDownload.h \
    ../../Protos/common.pb.h \
    Builder.h \
    priv/Constants.h \
    priv/OccupiedPeers.h \
    priv/Log.h \
    ../../Protos/queue.pb.h
