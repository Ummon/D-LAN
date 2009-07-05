# -------------------------------------------------
# Project created by QtCreator 2009-07-04T16:11:39
# -------------------------------------------------
QT -= gui
TARGET = 06_ConcurrentFileAccess
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    File.cpp \
    Chunk.cpp \
    Downloader.cpp \
    Uploader.cpp
HEADERS += File.h \
    Chunk.h \
    Downloader.h \
    Uploader.h
