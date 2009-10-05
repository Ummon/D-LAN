# -------------------------------------------------
# Project created by QtCreator 2009-10-05T18:53:58
# -------------------------------------------------
QT += network
QT -= gui
TARGET = UploadManager
INCLUDEPATH += .
TEMPLATE = lib

DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

DEFINES += UPLOADMANAGER_LIBRARY
SOURCES += priv/UploadManager.cpp \
    priv/Uploader.cpp \
    priv/Upload.cpp
HEADERS += UploadManager_global.h \
    IUploadManager.h \
    IUpload.h \
    priv/UploadManager.h \
    priv/Uploader.h \
    priv/Upload.h
