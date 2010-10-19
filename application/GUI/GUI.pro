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
    ../Protos/core_settings.pb.cc

HEADERS  += MainWindow.h \
    ../Protos/gui_protocol.pb.h \
    ../Protos/common.pb.h \
    PeerListModel.h \
    WidgetChat.h \
    CoreConnection.h \
    ../Protos/gui_settings.pb.h \
    ../Protos/core_settings.pb.h

FORMS    += MainWindow.ui \
    WidgetChat.ui

RESOURCES += \
    ressources.qrc
