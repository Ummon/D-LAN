# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib network
QT -= gui
TARGET = TestsCore
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../Libs/protobuf.pri)
include(../../Common/common.pri)

LIBS += -L../FileManager/output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += ../FileManager/output/$$FOLDER/libFileManager.a

LIBS += -L../PeerManager/output/$$FOLDER \
    -lPeerManager
POST_TARGETDEPS += ../PeerManager/output/$$FOLDER/libPeerManager.a

LIBS += -L../UploadManager/output/$$FOLDER \
    -lUploadManager
POST_TARGETDEPS += ../UploadManager/output/$$FOLDER/libUploadManager.a

LIBS += -L../DownloadManager/output/$$FOLDER \
    -lDownloadManager
POST_TARGETDEPS += ../DownloadManager/output/$$FOLDER/libDownloadManager.a

LIBS += -L../NetworkListener/output/$$FOLDER \
    -lNetworkListener
POST_TARGETDEPS += ../NetworkListener/output/$$FOLDER/libNetworkListener.a

LIBS += -L../RemoteControlManager/output/$$FOLDER \
    -lRemoteControlManager
POST_TARGETDEPS += ../RemoteControlManager/output/$$FOLDER/libRemoteControlManager.a

LIBS += -L../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

# FIXME : Theses declarations should not be here, all dependencies are read from the prl files of each library (see link_prl):
win32 {
    INCLUDEPATH += "."
    #INCLUDEPATH += "$$(QTDIR)/../mingw/include"
    #LIBS += "$$(QTDIR)/../mingw/lib/libwsock32.a"
    INCLUDEPATH += "C:/Qt/qtcreator-2.0.94/mingw/include"
    LIBS += "C:/Qt/qtcreator-2.0.94/mingw/lib/libwsock32.a"
}

INCLUDEPATH += . \
    .. \
    ../.. # For the 'Common' component.
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    CoreTest.cpp \
    ../../Protos/core_settings.pb.cc \
    ../Log.cpp \
    ../Core.cpp
HEADERS += Tests.h \
    CoreTest.h \
    ../../Protos/core_settings.pb.h \
    ../Log.h \
    ../Core.h

OTHER_FILES +=
