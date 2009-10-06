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
SOURCES += priv/WordIndex.cpp \
    priv/SharedDirectory.cpp \
    priv/FileUpdater.cpp \
    priv/FileManager.cpp \
    priv/File.cpp \
    priv/Directory.cpp \
    priv/Chunks.cpp \
    priv/Chunk.cpp \
    priv/Builder.cpp
HEADERS += FileManager_global.h \
    IGetHashesResult.h \
    IFileManager.h \
    IFile.h \
    IChunk.h \
    priv/WordIndex.h \
    priv/SharedDirectory.h \
    priv/FileUpdater.h \
    priv/FileManager.h \
    priv/File.h \
    priv/Directory.h \
    priv/Chunks.h \
    priv/Chunk.h \
    Builder.h
