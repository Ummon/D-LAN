# -------------------------------------------------
# Project created by QtCreator 2009-10-04T18:54:43
# -------------------------------------------------
QT -= gui
QT += network
TARGET = FileManager
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
 LIBS += -L../../Common/LogManager/output/$$FOLDER \
     -lLogManager
 POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a
 LIBS += -L../../Common/output/$$FOLDER \
     -lCommon
 POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

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
    priv/FileUpdater/WaitConditionWin.cpp \
    priv/FileUpdater/WaitConditionLinux.cpp
HEADERS += IGetHashesResult.h \
    IFileManager.h \
    IChunk.h \
    Builder.h \
    priv/Log.h \
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
    priv/FileUpdater/WaitConditionWin.h \
    priv/FileUpdater/WaitConditionLinux.h \
    priv/Constants.h
OTHER_FILES +=
