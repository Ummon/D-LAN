# -------------------------------------------------
# Project created by QtCreator 2009-10-04T18:54:43
# -------------------------------------------------
QT -= gui
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
DEFINES += FILEMANAGER_LIBRARY
SOURCES += priv/SharedDirectory.cpp \
    priv/FileUpdater/FileUpdater.cpp \
    priv/FileUpdater/DirWatcherWin.cpp \
    priv/FileUpdater/DirWatcher.cpp \
    priv/FileManager.cpp \
    priv/File.cpp \
    priv/Directory.cpp \
    priv/Chunks.cpp \
    priv/Chunk.cpp \
    priv/Builder.cpp \
    priv/Entry.cpp
HEADERS += FileManager_global.h \
    IGetHashesResult.h \
    IFileManager.h \
    IFile.h \
    IChunk.h \
    priv/SharedDirectory.h \
    priv/FileUpdater/FileUpdater.h \
    priv/FileUpdater/DirWatcherWin.h \
    priv/FileUpdater/DirWatcher.h \
    priv/FileManager.h \
    priv/File.h \
    priv/Directory.h \
    priv/Chunks.h \
    priv/Chunk.h \
    Builder.h \
    priv/Entry.h \
    priv/WordIndex/WordIndex.h \
    priv/WordIndex/Node.h
