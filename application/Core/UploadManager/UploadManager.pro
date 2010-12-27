# -------------------------------------------------
# Project created by QtCreator 2009-10-05T18:53:58
# -------------------------------------------------
QT += network
QT -= gui
TARGET = UploadManager
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


DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
DEFINES += UPLOADMANAGER_LIBRARY
SOURCES += priv/UploadManager.cpp \
    priv/Uploader.cpp \
    Builder.cpp \
    priv/Log.cpp
HEADERS += IUploadManager.h \
    IUpload.h \
    priv/UploadManager.h \
    priv/Uploader.h \
    Builder.h \
    priv/Constants.h \
    priv/Log.h
