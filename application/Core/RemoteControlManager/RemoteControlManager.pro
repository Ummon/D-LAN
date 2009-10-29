# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:08:20
# -------------------------------------------------
QT += network
QT -= gui
TARGET = RemoteControlManager
CONFIG += link_prl
INCLUDEPATH += . \
    ../..
TEMPLATE = lib
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
DEFINES += REMOTECONTROLMANAGER_LIBRARY
SOURCES += priv/RemoteControlManager.cpp \
    priv/RemoteConnection.cpp
HEADERS += IRemoteControlManager.h \
    priv/RemoteControlManager.h \
    priv/RemoteConnection.h
