# -------------------------------------------------
# Project created by QtCreator 2009-10-05T20:02:39
# -------------------------------------------------
QT += network
QT -= gui
TARGET = NetworkListener
TEMPLATE = lib

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

CONFIG += staticlib create_prl link_prl

INCLUDEPATH += . \
    ../..

DEFINES += NETWORKLISTENER_LIBRARY
SOURCES += priv/UDPListener.cpp \
    priv/TCPListener.cpp \
    priv/Search.cpp \
    priv/NetworkListener.cpp \
    priv/Chat.cpp \
    priv/Builder.cpp \
    ../../Protos/common.pb.cc \
    ../../Protos/core_protocol.pb.cc \
    priv/Log.cpp \
    priv/Utils.cpp
HEADERS += ISearch.h \
    INetworkListener.h \
    IChat.h \
    priv/UDPListener.h \
    priv/TCPListener.h \
    priv/Search.h \
    priv/NetworkListener.h \
    priv/Chat.h \
    Builder.h \
    ../../Protos/common.pb.h \
    ../../Protos/core_protocol.pb.h \
    priv/Log.h \
    priv/Utils.h
