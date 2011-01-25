# -------------------------------------------------
# Project created by QtCreator 2009-10-01T23:12:38
# -------------------------------------------------
QT -= gui
TARGET = LogManager

INCLUDEPATH += . .. ../..
TEMPLATE = lib
CONFIG += staticlib create_prl

include(../common.pri)

LIBS += -L../output/$$FOLDER -lCommon
POST_TARGETDEPS += ../output/$$FOLDER/libCommon.a

DEFINES += LOGMANAGER_LIBRARY

SOURCES += priv/Logger.cpp \
    priv/Entry.cpp \
    priv/Builder.cpp \
    priv/QtLogger.cpp \
    priv/StdLogger.cpp \
    priv/LoggerHook.cpp
HEADERS += ILogger.h \
    ILoggable.h \
    IEntry.h \
    Exceptions.h \
    Builder.h \
    priv/Logger.h \
    priv/Entry.h \
    priv/QtLogger.h \
    priv/StdLogger.h \
    priv/LoggerHook.h \
    ILoggerHook.h \
    LogMacros.h
