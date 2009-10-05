# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:20:26
# -------------------------------------------------
QT -= gui
TARGET = Core
INCLUDEPATH += . \
    .. \

LIBS += -L../Common/LogManager/output/debug \
   -lLogManager

DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Core.cpp
HEADERS += Core.h
