# -------------------------------------------------
# Project created by QtCreator 2009-10-12T19:57:21
# -------------------------------------------------
QT -= gui
TARGET = Common

include(common.pri)
include(../Libs/protobuf.pri)

INCLUDEPATH += . \
    ..
TEMPLATE = lib
CONFIG += staticlib \
    create_prl

INCLUDEPATH += . ..
DEFINES += COMMON_LIBRARY

SOURCES += Hash.cpp \
    Global.cpp \
    Network.cpp \
    ZeroCopyStreamQIODevice.cpp \
    Settings.cpp \
    TransferRateCalculator.cpp \
    ProtoHelper.cpp \
    Timeoutable.cpp \
    PersistentData.cpp \
    ../Libs/blake/blake_opt.c
HEADERS += Hashes.h \
    Hash.h \
    Constants.h \
    Global.h \
    Uncopyable.h \
    Network.h \
    ZeroCopyStreamQIODevice.h \
    Settings.h \
    TransferRateCalculator.h \
    ProtoHelper.h \
    Timeoutable.h \
    Version.h \
    PersistentData.h \
    ../Libs/blake/blake_opt.h \
    ../Libs/MersenneTwister.h \
    SharedDir.h
