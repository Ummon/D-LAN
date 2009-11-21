# -------------------------------------------------
# Project created by QtCreator 2009-10-12T19:57:21
# -------------------------------------------------
QT -= gui
TARGET = Common
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    .. \
    ${PROTOBUF}/src
TEMPLATE = lib
CONFIG += staticlib \
    create_prl
DEFINES += DEBUG
DESTDIR = "output/debug"
MOC_DIR = ".tmp/debug"
OBJECTS_DIR = ".tmp/debug"
INCLUDEPATH += .
DEFINES += COMMON_LIBRARY
SOURCES += Hash.cpp \
    PersistantData.cpp \
    Math.cpp \
    Global.cpp
HEADERS += Hashes.h \
    Hash.h \
    PersistantData.h \
    Math.h \
    Deletable.h \
    Constants.h \
    Global.h
