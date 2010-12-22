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
include(../Libs/qtservice/src/qtservice.pri)

INCLUDEPATH += . ..

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
    PeerListModel.cpp \
    WidgetChat.cpp \
    CoreConnection.cpp \
    ../Protos/gui_settings.pb.cc \
    ../Protos/core_settings.pb.cc \
    ChatModel.cpp \
    WidgetSettings.cpp \
    WidgetBrowse.cpp \
    WidgetSearch.cpp \
    BrowseModel.cpp \
    StatusBar.cpp \
    SearchModel.cpp \
    WidgetDownloads.cpp \
    DownloadsModel.cpp \
    WidgetUploads.cpp \
    UploadsModel.cpp \
    DialogAbout.cpp \
    LogModel.cpp \
    Log.cpp \
    AybabtuGUI.cpp \
    CheckBoxList.cpp \
    CoreController.cpp \
    TabButtons.cpp

HEADERS  += MainWindow.h \
    ../Protos/gui_protocol.pb.h \
    ../Protos/common.pb.h \
    PeerListModel.h \
    WidgetChat.h \
    CoreConnection.h \
    ../Protos/gui_settings.pb.h \
    ../Protos/core_settings.pb.h \
    ChatModel.h \
    WidgetSettings.h \
    WidgetBrowse.h \
    WidgetSearch.h \
    BrowseModel.h \
    StatusBar.h \
    SearchModel.h \
    Log.h \
    WidgetDownloads.h \
    DownloadsModel.h \
    UploadsModel.h \
    WidgetUploads.h \
    DialogAbout.h \
    LogModel.h \
    AybabtuGUI.h \
    CheckBoxList.h \
    DownloadFilterStatus.h \
    CheckBoxModel.h \
    IFilter.h \
    CoreController.h \
    TabButtons.h

FORMS    += MainWindow.ui \
    WidgetChat.ui \
    WidgetSettings.ui \
    WidgetBrowse.ui \
    WidgetSearch.ui \
    StatusBar.ui \
    WidgetDownloads.ui \
    WidgetUploads.ui \
    DialogAbout.ui

RESOURCES += \
    ressources.qrc
