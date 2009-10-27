# -------------------------------------------------
# Project created by QtCreator 2009-10-12T19:57:21
# -------------------------------------------------
QT -= gui
TARGET = Common

debug {
   DEFINES += DEBUG
   DESTDIR = "output/debug"
   MOC_DIR = ".tmp/debug"
   OBJECTS_DIR = ".tmp/debug"
} else {
   DEFINES += DEBUG
   DESTDIR = "output/release"
   MOC_DIR = ".tmp/release"
   OBJECTS_DIR = ".tmp/release"
}


INCLUDEPATH += .
TEMPLATE = lib
DEFINES += COMMON_LIBRARY
SOURCES += Hash.cpp \
    PersistantData.cpp \
    Math.cpp
HEADERS += Common_global.h \
    Hashes.h \
    Hash.h \
    PersistantData.h \
    Math.h \
    Deletable.h
