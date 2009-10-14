QT += testlib
QT += network
QT -= gui
TARGET = Tests
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"

#LIBS += -L../../output/debug \
#    -lNetworkManager

INCLUDEPATH += . \
    .. \ # NetworkListener
    ../.. \ # Core
    ../../.. \ # For access to Common and Protos
    ${PROTOBUF}/src

LIBS += -L../../../Common/LogManager/output/debug \
   -lLogManager

LIBS += -L../../../Common/output/debug \
   -lCommon

LIBS += -L../../FileManager/output/debug \
   -lFileManager

LIBS += -L../../NetworkListener/output/debug \
   -lNetworkListener

LIBS += -L../../PeerManager/output/debug \
   -lPeerManager

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp
HEADERS += Tests.h
