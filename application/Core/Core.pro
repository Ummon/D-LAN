# -------------------------------------------------
# Project created by QtCreator 2009-10-05T21:20:26
# -------------------------------------------------
QT += network
TARGET = AybabtuCore
CONFIG += link_prl

RC_FILE = ../Common/version.rc

include(../Common/common.pri)
include(../Libs/protobuf.pri)

INCLUDEPATH += . ..

LIBS += -LFileManager/output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += FileManager/output/$$FOLDER/libFileManager.a

LIBS += -LPeerManager/output/$$FOLDER \
    -lPeerManager
POST_TARGETDEPS += PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -LUploadManager/output/$$FOLDER \
    -lUploadManager
POST_TARGETDEPS += UploadManager/output/$$FOLDER/libUploadManager.a

LIBS += -LDownloadManager/output/$$FOLDER \
    -lDownloadManager
POST_TARGETDEPS += DownloadManager/output/$$FOLDER/libDownloadManager.a

LIBS += -LNetworkListener/output/$$FOLDER \
    -lNetworkListener
POST_TARGETDEPS += NetworkListener/output/$$FOLDER/libNetworkListener.a

LIBS += -LRemoteControlManager/output/$$FOLDER \
    -lRemoteControlManager
POST_TARGETDEPS += RemoteControlManager/output/$$FOLDER/libRemoteControlManager.a

LIBS += -L../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../Common/output/$$FOLDER/libCommon.a


include(../Libs/qtservice/src/qtservice.pri)


# FIXME : Theses declarations should not be here, all dependencies are read from the prl files of each library (see link_prl):
win32 {
    INCLUDEPATH += "."
    #INCLUDEPATH += "$$(QTDIR)/../mingw/include"
    #LIBS += "$$(QTDIR)/../mingw/lib/libwsock32.a"
    INCLUDEPATH += "C:/Qt/qtcreator-2.0.94/mingw/include"
    LIBS += "C:/Qt/qtcreator-2.0.94/mingw/lib/libwsock32.a"
}

CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    ../Protos/core_settings.pb.cc \
    Log.cpp \
    ConsoleReader.cpp \
    CoreService.cpp \
    Core.cpp
HEADERS += \
    Log.h \
    ../Protos/core_settings.pb.h \
    ConsoleReader.h \
    CoreService.h \
    Core.h

OTHER_FILES += \
    ../Libs/protobuf.pri

RESOURCES +=
