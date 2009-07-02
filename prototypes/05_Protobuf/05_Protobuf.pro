# -------------------------------------------------
# Project created by QtCreator 2009-06-28T12:29:04
# -------------------------------------------------
QT -= gui
TARGET = 05_Protobuf
INCLUDEPATH += "/usr/include/"

# LIBS += /usr/lib/libprotobuf.so # Windows style
LIBS += -L/usr/lib \
    -lprotobuf
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    addressbook.pb.cc
OTHER_FILES += addressbook.proto
HEADERS += addressbook.pb.h
