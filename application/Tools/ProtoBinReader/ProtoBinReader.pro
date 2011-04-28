#-------------------------------------------------
#
# Project created by QtCreator 2011-04-27T15:51:12
#
#-------------------------------------------------

QT       += core

QT       -= gui

TARGET = ProtoBinReader
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

INCLUDEPATH += . ../..

SOURCES += main.cpp \
    ../../Protos/queue.pb.cc \
    ../../Protos/files_cache.pb.cc \
    ../../Protos/common.pb.cc

HEADERS += \
    ../../Protos/queue.pb.h \
    ../../Protos/files_cache.pb.h \
    ../../Protos/common.pb.h
