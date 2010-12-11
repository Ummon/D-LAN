# -------------------------------------------------
# Project created by QtCreator 2009-06-21T15:26:58
# -------------------------------------------------
QT -= gui
TARGET = 11_BLAKE


CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    blake_opt.c
HEADERS += sha1.h \
    blake_opt.h
