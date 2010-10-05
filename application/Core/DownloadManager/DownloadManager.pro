# -------------------------------------------------
# Project created by QtCreator 2009-10-05T18:53:58
# -------------------------------------------------
QT += network
QT -= gui
TARGET = DownloadManager
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
LIBS += -L../FileManager/output/$$FOLDER \
     -lFileManager
POST_TARGETDEPS += ../FileManager/output/$$FOLDER/libFileManager.a
LIBS += -L../PeerManager/output/$$FOLDER \
     -lPeerManager
POST_TARGETDEPS += ../PeerManager/output/$$FOLDER/libPeerManager.a


CONFIG += staticlib create_prl link_prl
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
DEFINES += DOWNLOADMANAGER_LIBRARY

SOURCES += priv/FileDownload.cpp \
    priv/DownloadManager.cpp \
    priv/Download.cpp \
    priv/DirDownload.cpp \
    priv/ChunkDownloader.cpp \
    priv/ChunkDownload.cpp \
    ../../Protos/common.pb.cc \
    priv/Builder.cpp
HEADERS += IDownloadManager.h \
    IDownload.h \
    IChunkDownload.h \
    priv/FileDownload.h \
    priv/DownloadManager.h \
    priv/Download.h \
    priv/DirDownload.h \
    priv/ChunkDownloader.h \
    priv/ChunkDownload.h \
    ../../Protos/common.pb.h \
    Builder.h \
    priv/Constants.h
