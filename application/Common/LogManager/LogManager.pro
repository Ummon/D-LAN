# -------------------------------------------------
# Project created by QtCreator 2009-10-01T23:12:38
# -------------------------------------------------
QT -= gui
TARGET = LogManager
INCLUDEPATH += . \
   ..
TEMPLATE = lib
CONFIG += staticlib create_prl

debug {
    DEFINES += DEBUG
    DESTDIR = "output/debug"
    MOC_DIR = ".tmp/debug"
    OBJECTS_DIR = ".tmp/debug"
}
else {
    DEFINES += RELEASE
    DESTDIR = "output/release"
    MOC_DIR = ".tmp/relase"
    OBJECTS_DIR = ".tmp/release"
}
DEFINES += LOGMANAGER_LIBRARY

SOURCES += priv/Logger.cpp \
    priv/Entry.cpp \
    priv/Builder.cpp
HEADERS += ILogger.h \
    ILoggable.h \
    IEntry.h \
    Exceptions.h \
    Builder.h \
    priv/Logger.h \
    priv/Entry.h
