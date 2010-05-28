# -------------------------------------------------
# Project created by QtCreator 2009-10-01T23:12:38
# -------------------------------------------------
QT -= gui
TARGET = LogManager

INCLUDEPATH += . \
   ..
TEMPLATE = lib
CONFIG += staticlib create_prl

CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER

DEFINES += LOGMANAGER_LIBRARY

SOURCES += priv/Logger.cpp \
    priv/Entry.cpp \
    priv/Builder.cpp \
    priv/QtLogger.cpp \
    priv/StdLogger.cpp
HEADERS += ILogger.h \
    ILoggable.h \
    IEntry.h \
    Exceptions.h \
    Builder.h \
    priv/Logger.h \
    priv/Entry.h \
    priv/QtLogger.h \
    priv/StdLogger.h
