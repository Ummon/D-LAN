# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests
CONFIG += link_prl

include(../../common.pri)

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
