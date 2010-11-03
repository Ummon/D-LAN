#-------------------------------------------------
#
# Project created by QtCreator 2010-10-18T14:48:35
#
#-------------------------------------------------

QT       += core gui network
TARGET = AybabtuGUI
TEMPLATE = app

CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
#   CONFIG += console
} else {
   FOLDER = release
}

DESTDIR = output/$$FOLDER
MOC_DIR = ".tmp/$$FOLDER"
OBJECTS_DIR = ".tmp/$$FOLDER"
RCC_DIR = ".tmp/$$FOLDER"
UI_DIR = ".tmp/$$FOLDER"

INCLUDEPATH += . \
    .. \
    ${PROTOBUF}/src

LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf


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
    DownloadsModel.cpp

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
    DownloadsModel.h

FORMS    += MainWindow.ui \
    WidgetChat.ui \
    WidgetSettings.ui \
    WidgetBrowse.ui \
    WidgetSearch.ui \
    StatusBar.ui \
    WidgetDownloads.ui

RESOURCES += \
    ressources.qrc
