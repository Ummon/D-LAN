# -------------------------------------------------
# Project created by QtCreator 2009-10-04T18:54:43
# -------------------------------------------------
QT -= gui
QT += network
TARGET = FileManager
TEMPLATE = lib

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

CONFIG += staticlib link_prl create_prl

INCLUDEPATH += . ../..

DEFINES += FILEMANAGER_LIBRARY

win32 {
   SOURCES += priv/FileUpdater/WaitConditionWin.cpp
   HEADERS += priv/FileUpdater/DirWatcherWin.h
}

unix {
   SOURCES += priv/FileUpdater/WaitConditionLinux.cpp
   HEADERS += priv/FileUpdater/DirWatcherLinux.h
}

macx {
   SOURCES += priv/FileUpdater/WaitConditionDarwin.cpp
   HEADERS += priv/FileUpdater/DirWatcherDarwin.h
}

SOURCES += priv/Builder.cpp \
    priv/FileManager.cpp \
    priv/FileUpdater/FileUpdater.cpp \
    priv/FileUpdater/DirWatcherWin.cpp \
    priv/FileUpdater/DirWatcher.cpp \
    priv/Cache/Entry.cpp \
    priv/Cache/File.cpp \
    priv/Cache/Directory.cpp \
    priv/Cache/SharedDirectory.cpp \
    priv/ChunkIndex/Chunks.cpp \
    ../../Protos/core_protocol.pb.cc \
    ../../Protos/common.pb.cc \
    priv/Cache/Chunk.cpp \
    priv/Cache/DataReader.cpp \
    priv/Cache/DataWriter.cpp \
    priv/Cache/Cache.cpp \
    ../../Protos/files_cache.pb.cc \
    priv/FileUpdater/WaitCondition.cpp \
    priv/GetHashesResult.cpp \
    priv/Log.cpp \
    priv/Global.cpp \
    priv/FileUpdater/DirWatcherLinux.cpp \
    priv/Cache/FilePool.cpp \
    priv/Cache/FileHasher.cpp
HEADERS += IGetHashesResult.h \
    IFileManager.h \
    IChunk.h \
    Builder.h \
    priv/Log.h \
    priv/FileManager.h \
    priv/FileUpdater/FileUpdater.h \
    priv/FileUpdater/DirWatcher.h \
    priv/Cache/Entry.h \
    priv/Cache/File.h \
    priv/Cache/Directory.h \
    priv/Cache/SharedDirectory.h \
    priv/ChunkIndex/Chunks.h \
    priv/WordIndex/WordIndex.h \
    priv/WordIndex/Node.h \
    ../../Protos/core_protocol.pb.h \
    ../../Protos/common.pb.h \
    IDataReader.h \
    IDataWriter.h \
    priv/Cache/Chunk.h \
    priv/Cache/DataReader.h \
    priv/Cache/DataWriter.h \
    priv/Cache/Cache.h \
    priv/Exceptions.h \
    Exceptions.h \
    ../../Protos/files_cache.pb.h \
    priv/FileUpdater/WaitCondition.h \
    priv/Constants.h \
    priv/GetHashesResult.h \
    priv/Global.h \
    priv/FileUpdater/DirWatcherLinux.h \
    priv/Cache/FilePool.h \
    priv/Cache/FileHasher.h
OTHER_FILES +=
