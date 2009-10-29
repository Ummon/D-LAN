# -------------------------------------------------
# Project created by QtCreator 2009-10-12T19:57:21
# -------------------------------------------------
QT -= gui
TARGET = Common
TEMPLATE = lib
CONFIG += staticlib create_prl
DEFINES += DEBUG
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
INCLUDEPATH += .
DEFINES += COMMON_LIBRARY
SOURCES += Hash.cpp \
    PersistantData.cpp \
    Math.cpp
HEADERS += Hashes.h \
    Hash.h \
    PersistantData.h \
    Math.h \
    Deletable.h
