# -------------------------------------------------
# Project created by QtCreator 2009-10-05T19:14:23
# -------------------------------------------------
QT += network
QT -= gui
TARGET = PeerManager
TEMPLATE = lib
CONFIG += staticlib create_prl link_prl
INCLUDEPATH += . \
    ../.. \ # For Common and LogManager.
    ${PROTOBUF}/src
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
LIBS += -L../../Common/output/debug \
    -lCommon
LIBS += -L../../Common/LogManager/output/debug \
    -lLogManager
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
DEFINES += PEERMANAGER_LIBRARY
SOURCES += priv/PeerManager.cpp \
    priv/Peer.cpp \
    priv/Builder.cpp
HEADERS += IPeerManager.h \
    IPeer.h \
    IGetEntries.h \
    priv/PeerManager.h \
    priv/Peer.h \
    Builder.h
