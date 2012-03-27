# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:20:26
# -------------------------------------------------

# Uncomment this line to enable the leak detector.
# DEFINES += ENABLE_NVWA

QT += network
TARGET = "D-LAN.Core"
CONFIG += link_prl

RC_FILE = ../Common/version.rc

include(../Common/common.pri)
include(../Libs/protobuf.pri)

INCLUDEPATH += . ..

LIBS += -LRemoteControlManager/output/$$FOLDER \
    -lRemoteControlManager
PRE_TARGETDEPS += RemoteControlManager/output/$$FOLDER/libRemoteControlManager.a

LIBS += -LNetworkListener/output/$$FOLDER \
    -lNetworkListener
PRE_TARGETDEPS += NetworkListener/output/$$FOLDER/libNetworkListener.a

LIBS += -LDownloadManager/output/$$FOLDER \
    -lDownloadManager
PRE_TARGETDEPS += DownloadManager/output/$$FOLDER/libDownloadManager.a

LIBS += -LUploadManager/output/$$FOLDER \
    -lUploadManager
PRE_TARGETDEPS += UploadManager/output/$$FOLDER/libUploadManager.a

LIBS += -LPeerManager/output/$$FOLDER \
    -lPeerManager
PRE_TARGETDEPS += PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -LFileManager/output/$$FOLDER \
    -lFileManager
PRE_TARGETDEPS += FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../Common/LogManager/output/$$FOLDER \
    -lLogManager
PRE_TARGETDEPS += ../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../Common/output/$$FOLDER \
    -lCommon
PRE_TARGETDEPS += ../Common/output/$$FOLDER/libCommon.a

include(../Libs/qtservice/src/qtservice.pri)

win32 {
   # The NetworkListener component use some functions from this lib.
   # See the 'UDPListener::initMulticastUDPSocket()' method.
   LIBS += -lwsock32
}

CONFIG(debug, debug|release) {
   contains(DEFINES, ENABLE_NVWA) {
      DEFINES += _DEBUG_NEW_ERROR_CRASH
      SOURCES += ../Libs/Nvwa/debug_new.cpp
   }
}

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    ../Protos/core_settings.pb.cc \
    Log.cpp \
    ConsoleReader.cpp \
    CoreService.cpp \
    Core.cpp \
    CoreApplication.cpp
HEADERS += \
    Log.h \
    ../Protos/core_settings.pb.h \
    ConsoleReader.h \
    CoreService.h \
    Core.h \
    CoreApplication.h

OTHER_FILES += \
    ../Libs/protobuf.pri

RESOURCES +=


