# -------------------------------------------------
# Project created by QtCreator 2009-06-26T21:24:50
# -------------------------------------------------
QT += network
QT -= gui

win32 {
   INCLUDEPATH += "."
   LIBS += "$$(WIN_PLATFORM_SDK)\Lib\WSOCK32.LIB"
}

TARGET = 04_UDP
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Chat.cpp
HEADERS += Chat.h
