# -------------------------------------------------
# Project created by QtCreator 2009-10-05T18:53:58
# -------------------------------------------------
QT += network
QT -= gui
TARGET = UploadManager
INCLUDEPATH += .
TEMPLATE = lib
CONFIG += staticlib create_prl link_prl
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
DEFINES += UPLOADMANAGER_LIBRARY
SOURCES += priv/UploadManager.cpp \
    priv/Uploader.cpp \
    priv/Upload.cpp
HEADERS += IUploadManager.h \
    IUpload.h \
    priv/UploadManager.h \
    priv/Uploader.h \
    priv/Upload.h
