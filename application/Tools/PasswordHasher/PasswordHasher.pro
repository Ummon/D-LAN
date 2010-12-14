#-------------------------------------------------
#
# Project created by QtCreator 2010-12-13T14:40:25
#
#-------------------------------------------------

QT       += core gui

TARGET = PasswordHasher
TEMPLATE = app

INCLUDEPATH += . ../..

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

LIBS += -L../../Common/output/$$FOLDER -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

SOURCES += main.cpp MainWindow.cpp \
    ../../Protos/common.pb.cc
HEADERS  += MainWindow.h \
    ../../Protos/common.pb.h
FORMS    += MainWindow.ui
