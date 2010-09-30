# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests
CONFIG += link_prl

CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

DESTDIR = "output/$$FOLDER"
MOC_DIR = ".tmp/$$FOLDER"
OBJECTS_DIR = ".tmp/$$FOLDER"

LIBS += -L../output/$$FOLDER \
   -lLogManager
POST_TARGETDEPS += ../output/$$FOLDER/libLogManager.a

LIBS += -L../../output/$$FOLDER \
   -lCommon
POST_TARGETDEPS += ../../output/$$FOLDER/libCommon.a

INCLUDEPATH += . \
   .. \
   ../../.. # For the 'Common' component.
CONFIG += console
CONFIG -= app_bundle
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp
HEADERS += Tests.h
