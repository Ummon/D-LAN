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

DEFINES += DOWNLOADMANAGER_LIBRARY

SOURCES += priv/FileDownload.cpp \
    priv/DownloadManager.cpp \
    priv/Download.cpp \
    priv/DirDownload.cpp \
    priv/ChunkDownload.cpp \
    ../../Protos/common.pb.cc \
    priv/Builder.cpp \
    priv/OccupiedPeers.cpp \
    ../../Protos/queue.pb.cc \
    priv/Log.cpp \
    priv/DownloadPredicate.cpp \
    priv/DownloadQueue.cpp \
    Utils.cpp
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
    ../../Protos/queue.pb.h \
    ../../Libs/MersenneTwister.h \
    priv/DownloadPredicate.h \
    priv/DownloadQueue.h \
    Utils.h \
    priv/LinkedPeers.h
