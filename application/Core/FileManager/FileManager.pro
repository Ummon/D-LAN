# -------------------------------------------------
# Project created by QtCreator 2009-10-04T18:54:43
# -------------------------------------------------
QT -= gui
QT += network
TARGET = FileManager
TEMPLATE = lib

LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf

INCLUDEPATH += . \
    ../.. \
    ${PROTOBUF}/src

debug {
   DEFINES += DEBUG
   DESTDIR = "output/debug"
   MOC_DIR = ".tmp/debug"
   OBJECTS_DIR = ".tmp/debug"
   LIBS += -L../../Common/LogManager/output/debug \
       -lLogManager
   LIBS += -L../../Common/output/debug \
       -lCommon
}

release {
   DEFINES += RELEASE
   DESTDIR = "output/release"
   MOC_DIR = ".tmp/release"
   OBJECTS_DIR = ".tmp/release"
   LIBS += -L../../Common/LogManager/output/release \
       -lLogManager
   LIBS += -L../../Common/output/release \
       -lCommon
}


DEFINES += FILEMANAGER_LIBRARY
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
    priv/FileUpdater/WaitConditionWin.cpp
HEADERS += priv/FileManager_global.h \
    IGetHashesResult.h \
    IFileManager.h \
    IChunk.h \
    Builder.h \
    priv/FileManager.h \
    priv/FileUpdater/FileUpdater.h \
    priv/FileUpdater/DirWatcherWin.h \
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
    priv/FileUpdater/WaitConditionWin.h
OTHER_FILES +=
