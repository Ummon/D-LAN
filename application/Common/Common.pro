# -------------------------------------------------
# Project created by QtCreator 2009-10-12T19:57:21
# -------------------------------------------------
QT -= gui
TARGET = Common

CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    .. \
    ${PROTOBUF}/src
TEMPLATE = lib
CONFIG += staticlib \
    create_prl
DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER
INCLUDEPATH += .
DEFINES += COMMON_LIBRARY
SOURCES += Hash.cpp \
    PersistantData.cpp \
    Global.cpp \
    Network.cpp \
    ZeroCopyStreamQIODevice.cpp \
    Settings.cpp \
    TransferRateCalculator.cpp \
    ProtoHelper.cpp \
    Timeoutable.cpp
HEADERS += Hashes.h \
    Hash.h \
    PersistantData.h \
    Deletable.h \
    Constants.h \
    Global.h \
    Uncopyable.h \
    Network.h \
    ZeroCopyStreamQIODevice.h \
    Settings.h \
    TransferRateCalculator.h \
    ProtoHelper.h \
    Timeoutable.h
