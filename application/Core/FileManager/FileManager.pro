# -------------------------------------------------
# Project created by QtCreator 2009-10-04T18:54:43
# -------------------------------------------------
QT -= gui
QT += network
TARGET = FileManager
TEMPLATE = lib
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
INCLUDEPATH += . \
    ../.. \
    ${PROTOBUF}/src
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
LIBS += -L../../Common/LogManager/output/debug \
    -lLogManager
LIBS += -L../../Common/output/debug \
    -lCommon
DEFINES += FILEMANAGER_LIBRARY
debug:DEFINES += DEBUG
release:DEFINES += RELEASE
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
    priv/Cache/Cache.cpp
HEADERS += FileManager_global.h \
    IGetHashesResult.h \
    IFileManager.h \
    IFile.h \
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
    Exceptions.h
OTHER_FILES +=
