#-------------------------------------------------
#
# Project created by QtCreator 2010-11-22T17:04:09
#
#-------------------------------------------------

QT       += core network

QT       -= gui

TARGET = 10_ThreadedTransfer
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

INCLUDEPATH += .

SOURCES += main.cpp \
    Downloader.cpp \
    Uploader.cpp \
    Listener.cpp

HEADERS += \
    Downloader.h \
    Uploader.h \
    Listener.h
