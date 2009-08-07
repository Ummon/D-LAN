# -------------------------------------------------
# Project created by QtCreator 2009-08-03T20:42:38
# -------------------------------------------------
QT -= gui
win32 { 
    INCLUDEPATH += "."
    INCLUDEPATH += "$$(QTDIR)\..\mingw\include"
}

TARGET = 07_Watcher
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    DirWatcher.cpp \
    DirWatcherWin.cpp
HEADERS += DirWatcher.h \
    DirWatcherWin.h
