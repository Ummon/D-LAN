# -------------------------------------------------
# Project created by QtCreator 2009-06-26T21:24:50
# -------------------------------------------------
QT += network
QT -= gui

win32 {
   INCLUDEPATH += "."
   INCLUDEPATH += "$$(QTDIR)\..\mingw\include"
   LIBS += "$$(QTDIR)\..\mingw\lib\libwsock32.a"
}

TARGET = 04_UDP
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Chat.cpp
HEADERS += Chat.h
