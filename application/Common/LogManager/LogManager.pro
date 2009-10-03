# -------------------------------------------------
# Project created by QtCreator 2009-10-01T23:12:38
# -------------------------------------------------
QT -= gui
TARGET = LogManager
INCLUDEPATH += .
TEMPLATE = lib
CONFIG += staticlib
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
    priv/Entry.h

