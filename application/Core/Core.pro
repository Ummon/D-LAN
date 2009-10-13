# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:20:26
# -------------------------------------------------
QT -= gui
QT += network
TARGET = Core
INCLUDEPATH += . \
    .. \
    ${PROTOBUF}/src

LIBS += -L../Common/LogManager/output/debug \
   -lLogManager


LIBS += -L../Common/output/debug \
   -lCommon

LIBS += -LFileManager/output/debug \
   -lFileManager

LIBS += -LNetworkListener/output/debug \
   -lNetworkListener

LIBS += -LPeerManager/output/debug \
   -lPeerManager

DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Core.cpp
HEADERS += Core.h
