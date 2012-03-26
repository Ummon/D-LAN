#-------------------------------------------------
#
# Project created by QtCreator 2010-10-18T14:48:35
#
#-------------------------------------------------

# Uncomment this line to enable the leak detector.
# DEFINES += ENABLE_NVWA

QT       += core gui network
TARGET = "D-LAN.GUI"
TEMPLATE = app

RC_FILE = ../Common/version.rc

include(../Common/common.pri)
include(../Libs/protobuf.pri)

INCLUDEPATH += . ..

LIBS += -L../Common/RemoteCoreController/output/$$FOLDER \
    -lRemoteCoreController
PRE_TARGETDEPS += ../Common/RemoteCoreController/output/$$FOLDER/libRemoteCoreController.a

LIBS += -L../Common/LogManager/output/$$FOLDER \
    -lLogManager
PRE_TARGETDEPS += ../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../Common/output/$$FOLDER \
    -lCommon
PRE_TARGETDEPS += ../Common/output/$$FOLDER/libCommon.a

CONFIG(debug, debug|release) {
   contains(DEFINES, ENABLE_NVWA) {
      DEFINES += _DEBUG_NEW_ERROR_CRASH
      SOURCES += ../Libs/Nvwa/debug_new.cpp
   }
}

SOURCES += main.cpp\
    MainWindow.cpp \
    ../Protos/gui_protocol.pb.cc \
    ../Protos/common.pb.cc \
    ../Protos/gui_settings.pb.cc \
    ../Protos/core_settings.pb.cc \
    StatusBar.cpp \
    DialogAbout.cpp \
    Log.cpp \
    CheckBoxList.cpp \
    TabButtons.cpp \
    Browse/WidgetBrowse.cpp \
    Browse/BrowseModel.cpp \
    Chat/WidgetChat.cpp \
    Chat/ChatModel.cpp \
    Downloads/WidgetDownloads.cpp \
    Downloads/DownloadsFlatModel.cpp \
    Log/LogModel.cpp \
    PeerList/PeerListModel.cpp \
    Search/WidgetSearch.cpp \
    Search/SearchModel.cpp \
    Settings/WidgetSettings.cpp \
    Uploads/WidgetUploads.cpp \
    Uploads/UploadsModel.cpp \
    Settings/DirListModel.cpp \
    Settings/RemoteFileDialog.cpp \
    DownloadMenu.cpp \
    D-LAN_GUI.cpp \
    ProgressBar.cpp \
    IconProvider.cpp \
    Utils.cpp \
    LineEdit.cpp \
    Settings/AskNewPasswordDialog.cpp \
    Downloads/DownloadsTreeModel.cpp \
    Downloads/DownloadsModel.cpp

HEADERS  += MainWindow.h \
    ../Protos/gui_protocol.pb.h \
    ../Protos/common.pb.h \
    ../Protos/gui_settings.pb.h \
    ../Protos/core_settings.pb.h \
    StatusBar.h \
    Log.h \
    DialogAbout.h \
    CheckBoxList.h \
    CheckBoxModel.h \
    IFilter.h \
    TabButtons.h \
    Browse/WidgetBrowse.h \
    Browse/BrowseModel.h \
    Chat/WidgetChat.h \
    Chat/ChatModel.h \
    Downloads/WidgetDownloads.h \
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
    DownloadMenu.h \
    D-LAN_GUI.h \
    ProgressBar.h \
    IconProvider.h \
    Utils.h \
    LineEdit.h \
    Settings/AskNewPasswordDialog.h \
    Downloads/DownloadsFlatModel.h \
    Downloads/DownloadsTreeModel.h \
    Downloads/DownloadsModel.h

FORMS    += MainWindow.ui \
    StatusBar.ui \
    DialogAbout.ui \
    Browse/WidgetBrowse.ui \
    Chat/WidgetChat.ui \
    Downloads/WidgetDownloads.ui \
    Search/WidgetSearch.ui \
    Settings/WidgetSettings.ui \
    Uploads/WidgetUploads.ui \
    Settings/RemoteFileDialog.ui \
    Settings/AskNewPasswordDialog.ui

RESOURCES += \
    ressources.qrc


