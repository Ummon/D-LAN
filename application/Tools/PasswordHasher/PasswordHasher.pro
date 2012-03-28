#-------------------------------------------------
#
# Project created by QtCreator 2010-12-13T14:40:25
#
#-------------------------------------------------

QT       += core network gui

TARGET = PasswordHasher
TEMPLATE = app

INCLUDEPATH += . ../..

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

LIBS += -L../../Common/output/$$FOLDER -lCommon
PRE_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

SOURCES += main.cpp MainWindow.cpp \
    ../../Protos/common.pb.cc \
    ../../Protos/core_settings.pb.cc
HEADERS  += MainWindow.h \
    ../../Protos/common.pb.h \
    ../../Protos/core_settings.pb.h
FORMS    += MainWindow.ui

RC_FILE = passwordhasher.rc

RESOURCES += \
    ressources.qrc
