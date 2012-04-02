# -------------------------------------------------
# Project created by QtCreator 2009-10-12T19:57:21
# -------------------------------------------------
QT -= gui
QT += network
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
    ZeroCopyStreamQIODevice.cpp \
    Settings.cpp \
    TransferRateCalculator.cpp \
    ProtoHelper.cpp \
    Timeoutable.cpp \
    PersistentData.cpp \
    Network/MessageSocket.cpp \
    Network/MessageHeader.cpp \
    ../Protos/gui_protocol.pb.cc \
    ../Protos/core_protocol.pb.cc \
    ../Protos/common.pb.cc \
    ThreadPool.cpp \
    Languages.cpp \
    Constants.cpp \
    FileLocker.cpp

HEADERS += Hashes.h \
    Hash.h \
    Constants.h \
    Global.h \
    Uncopyable.h \
    ZeroCopyStreamQIODevice.h \
    Settings.h \
    TransferRateCalculator.h \
    ProtoHelper.h \
    Timeoutable.h \
    Version.h \
    PersistentData.h \
    ../Libs/MersenneTwister.h \
    SharedDir.h \
    Network/MessageSocket.h \
    Network/MessageHeader.h \
    ../Protos/gui_protocol.pb.h \
    ../Protos/core_protocol.pb.h \
    ../Protos/common.pb.h \
    ThreadPool.h \
    IRunnable.h \
    Languages.h \
    Tree.h \
    FileLocker.h


