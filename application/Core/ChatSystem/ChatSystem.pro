# -------------------------------------------------
# Project created by QtCreator 2009-10-05T18:53:58
# -------------------------------------------------
QT += network
QT -= gui
TARGET = ChatSystem
TEMPLATE = lib

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

CONFIG += staticlib \
    link_prl \
    create_prl

INCLUDEPATH += . \
    ../..

DEFINES += CHATSYSTEM_LIBRARY
SOURCES +=  Builder.cpp \
    priv/Log.cpp \
    priv/ChatSystem.cpp \
    priv/ChatMessage.cpp \
    priv/ChatMessages.cpp
HEADERS += IChatSystem.h \
    priv/ChatSystem.h \
    Builder.h \
    priv/Log.h \
    priv/ChatMessage.h \
    priv/ChatMessages.h
