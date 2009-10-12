# -------------------------------------------------
# Project created by QtCreator 2009-10-05T20:02:39
# -------------------------------------------------
QT += network
QT -= gui
TARGET = NetworkListener
INCLUDEPATH += . \
    ../.. \
    .. \
    ${PROTOBUF}/src


LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf

LIBS += -L../../Common/LogManager/output/debug \
   -lLogManager

LIBS += -L../PeerManager/output/debug \
   -lPeerManager

TEMPLATE = lib
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
DEFINES += NETWORKLISTENER_LIBRARY
SOURCES += priv/UDPListener.cpp \
    priv/TCPListener.cpp \
    priv/Search.cpp \
    priv/NetworkListener.cpp \
    priv/ChunkUpdater.cpp \
    priv/Chat.cpp \
    priv/Builder.cpp \
    ../../Protos/common.pb.cc \
    ../../Protos/core_protocol.pb.cc
HEADERS += NetworkListener_global.h \
    ISearch.h \
    INetworkListener.h \
    IChat.h \
    priv/UDPListener.h \
    priv/TCPListener.h \
    priv/Search.h \
    priv/NetworkListener.h \
    priv/ChunkUpdater.h \
    priv/Chat.h \
    Builder.h \
    ../../Protos/common.pb.h \
    ../../Protos/core_protocol.pb.h
