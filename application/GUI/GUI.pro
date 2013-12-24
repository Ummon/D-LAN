#-------------------------------------------------
#
# Project created by QtCreator 2010-10-18T14:48:35
#
#-------------------------------------------------

# Uncomment this line to enable the leak detector.
# DEFINES += ENABLE_NVWA

QT       += core gui network xml
TARGET = "D-LAN.GUI"
TEMPLATE = app

RC_FILE = ../Common/version.rc

include(../Common/common.pri)
include(../Libs/protobuf.pri)
include(../Protos/Protos.pri)

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

win32 {
   LIBS += libole32
   SOURCES += Taskbar/TaskbarImplWin.cpp
   HEADERS += Taskbar/TaskbarImplWin.h
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
    Browse/BrowseModel.cpp \
    Chat/ChatModel.cpp \
    Downloads/DownloadsFlatModel.cpp \
    Log/LogModel.cpp \
    Search/SearchModel.cpp \
    Uploads/UploadsModel.cpp \
    Settings/DirListModel.cpp \
    Settings/RemoteFileDialog.cpp \
    DownloadMenu.cpp \
    D-LAN_GUI.cpp \
    ProgressBar.cpp \
    IconProvider.cpp \
    Utils.cpp \
    Settings/AskNewPasswordDialog.cpp \
    Downloads/DownloadsTreeModel.cpp \
    Downloads/DownloadsModel.cpp \
    BusyIndicator.cpp \
    Log/LogDelegate.cpp \
    Chat/RoomsModel.cpp \
    Chat/RoomsDelegate.cpp \
    Peers/PeersDock.cpp \
    Peers/PeerListModel.cpp \
    Peers/PeerListDelegate.cpp \
    Chat/RoomsDock.cpp \
    Search/SearchDock.cpp \
    MDI/TabButtons.cpp \
    MDI/MdiArea.cpp \
    Browse/BrowseWidget.cpp \
    Chat/ChatWidget.cpp \
    Downloads/DownloadsWidget.cpp \
    Search/SearchWidget.cpp \
    Settings/SettingsWidget.cpp \
    Uploads/UploadsWidget.cpp \
    MDI/MdiWidget.cpp \
    ColorBox.cpp \
    Chat/ChatTextEdit.cpp \
    Constants.cpp \
    Emoticons/EmoticonsWidget.cpp \
    Emoticons/Emoticons.cpp \
    Emoticons/SingleEmoticonWidget.cpp \
    AutoComplete/AutoComplete.cpp \
    AutoComplete/AutoCompleteModel.cpp \
    Search/SearchUtils.cpp

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
    Browse/BrowseModel.h \
    Chat/ChatModel.h \
    Downloads/DownloadFilterStatus.h \
    Log/LogModel.h \
    Search/SearchModel.h \
    Uploads/UploadsModel.h \
    Settings/DirListModel.h \
    Settings/RemoteFileDialog.h \
    DownloadMenu.h \
    D-LAN_GUI.h \
    ProgressBar.h \
    IconProvider.h \
    Utils.h \
    Settings/AskNewPasswordDialog.h \
    Downloads/DownloadsFlatModel.h \
    Downloads/DownloadsTreeModel.h \
    Downloads/DownloadsModel.h \
    BusyIndicator.h \
    Taskbar/Taskbar.h \
    Taskbar/ITaskbarImpl.h \
    Taskbar/WinUtil.h \
    Taskbar/TaskbarTypes.h \
    Log/LogDelegate.h \
    Chat/RoomsModel.h \
    Chat/RoomsDelegate.h \
    Peers/PeersDock.h \
    Peers/PeerListModel.h \
    Peers/PeerListDelegate.h \
    Chat/RoomsDock.h \
    Search/SearchDock.h \
    MDI/TabButtons.h \
    MDI/MdiArea.h \
    MDI/MdiWidget.h \
    Browse/BrowseWidget.h \
    Chat/ChatWidget.h \
    Downloads/DownloadsWidget.h \
    Search/SearchWidget.h \
    Settings/SettingsWidget.h \
    Uploads/UploadsWidget.h \
    ColorBox.h \
    Chat/ChatTextEdit.h \
    Constants.h \
    Emoticons/EmoticonsWidget.h \
    Emoticons/Emoticons.h \
    Emoticons/SingleEmoticonWidget.h \
    AutoComplete/AutoComplete.h \
    AutoComplete/AutoCompleteModel.h \
    Search/SearchUtils.h

FORMS    += MainWindow.ui \
    StatusBar.ui \
    DialogAbout.ui \
    Settings/RemoteFileDialog.ui \
    Settings/AskNewPasswordDialog.ui \
    Peers/PeersDock.ui \
    Chat/RoomsDock.ui \
    Search/SearchDock.ui \
    Browse/BrowseWidget.ui \
    Chat/ChatWidget.ui \
    Downloads/DownloadsWidget.ui \
    Search/SearchWidget.ui \
    Settings/SettingsWidget.ui \
    Uploads/UploadsWidget.ui \
    Emoticons/SingleEmoticonWidget.ui \
    AutoComplete/AutoComplete.ui

RESOURCES += \
    ressources.qrc


