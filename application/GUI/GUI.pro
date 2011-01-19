#-------------------------------------------------
#
# Project created by QtCreator 2010-10-18T14:48:35
#
#-------------------------------------------------

QT       += core gui network
TARGET = AybabtuGUI
TEMPLATE = app

RC_FILE = ../Common/version.rc

include(../Common/common.pri)
include(../Libs/protobuf.pri)

INCLUDEPATH += . ..

LIBS += -L../Common/RemoteCoreController/output/$$FOLDER \
    -lRemoteCoreController
POST_TARGETDEPS += ../Common/RemoteCoreController/output/$$FOLDER/libRemoteCoreController.a

LIBS += -L../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../Common/output/$$FOLDER/libCommon.a

SOURCES += main.cpp\
        MainWindow.cpp \
    ../Protos/gui_protocol.pb.cc \
    ../Protos/common.pb.cc \
    ../Protos/gui_settings.pb.cc \
    ../Protos/core_settings.pb.cc \
    StatusBar.cpp \
    DialogAbout.cpp \
    Log.cpp \
    AybabtuGUI.cpp \
    CheckBoxList.cpp \
    TabButtons.cpp \
    Browse/WidgetBrowse.cpp \
    Browse/BrowseModel.cpp \
    Chat/WidgetChat.cpp \
    Chat/ChatModel.cpp \
    Downloads/WidgetDownloads.cpp \
    Downloads/DownloadsModel.cpp \
    Log/LogModel.cpp \
    PeerList/PeerListModel.cpp \
    Search/WidgetSearch.cpp \
    Search/SearchModel.cpp \
    Settings/WidgetSettings.cpp \
    Uploads/WidgetUploads.cpp \
    Uploads/UploadsModel.cpp \
    Settings/DirListModel.cpp \
    Settings/RemoteFileDialog.cpp \
    DownloadMenu.cpp

HEADERS  += MainWindow.h \
    ../Protos/gui_protocol.pb.h \
    ../Protos/common.pb.h \
    ../Protos/gui_settings.pb.h \
    ../Protos/core_settings.pb.h \
    StatusBar.h \
    Log.h \
    DialogAbout.h \
    AybabtuGUI.h \
    CheckBoxList.h \
    CheckBoxModel.h \
    IFilter.h \
    TabButtons.h \
    Browse/WidgetBrowse.h \
    Browse/BrowseModel.h \
    Chat/WidgetChat.h \
    Chat/ChatModel.h \
    Downloads/WidgetDownloads.h \
    Downloads/DownloadsModel.h \
    Downloads/DownloadFilterStatus.h \
    Log/LogModel.h \
    PeerList/PeerListModel.h \
    Search/WidgetSearch.h \
    Search/SearchModel.h \
    Settings/WidgetSettings.h \
    Uploads/WidgetUploads.h \
    Uploads/UploadsModel.h \
    Settings/DirListModel.h \
    Settings/RemoteFileDialog.h \
    DownloadMenu.h

FORMS    += MainWindow.ui \
    StatusBar.ui \
    DialogAbout.ui \
    Browse/WidgetBrowse.ui \
    Chat/WidgetChat.ui \
    Downloads/WidgetDownloads.ui \
    Search/WidgetSearch.ui \
    Settings/WidgetSettings.ui \
    Uploads/WidgetUploads.ui \
    Settings/RemoteFileDialog.ui

RESOURCES += \
    ressources.qrc
