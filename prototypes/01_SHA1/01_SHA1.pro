# -------------------------------------------------
# Project created by QtCreator 2009-06-21T15:26:58
# -------------------------------------------------
QT -= gui
TARGET = 01_SHA1

# For ntohl() and htonl() needed by sha1.c.
win32 {
    LIBS += -L$$(QTDIR)/../mingw/lib -lws2_32
}

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    sha1.c
HEADERS += sha1.h
