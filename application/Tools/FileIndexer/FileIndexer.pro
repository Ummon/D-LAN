#-------------------------------------------------
#
# Project created by QtCreator 2012-04-25T21:53:31
#
#-------------------------------------------------

QT       += core network
QT       -= gui

TARGET = FileIndexer
CONFIG   += console #prof
CONFIG   -= app_bundle

TEMPLATE = app

include(../../Common/common.pri)

INCLUDEPATH += . ../.. ../../Core/FileManager

LIBS += -L../../Common/output/$$FOLDER \
    -lCommon
PRE_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

SOURCES += main.cpp

HEADERS += \
    ../../Core/FileManager/priv/WordIndex/WordIndex.h \
    ../../Core/FileManager/priv/WordIndex/Node.h \
    OldWordIndex.h \
    OldNode.h
