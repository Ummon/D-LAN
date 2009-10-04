# -------------------------------------------------
# Project created by QtCreator 2009-10-01T23:12:38
# -------------------------------------------------
QT -= gui
TARGET = LogManager  
INCLUDEPATH += .

TEMPLATE = lib

# Automatically well defined for realse mode too.
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

DEFINES += LOGMANAGER_LIBRARY
CONFIG += shared
SOURCES += priv/LogManager.cpp \
    priv/Logger.cpp \
    priv/Entry.cpp \
    priv/Builder.cpp
HEADERS += ILogManager.h \
    ILogger.h \
    ILoggable.h \
    IEntry.h \
    Exceptions.h \
    Builder.h \
    priv/LogManager.h \
    priv/Logger.h \
    priv/Entry.h \
    LogManagerGlobal.h
