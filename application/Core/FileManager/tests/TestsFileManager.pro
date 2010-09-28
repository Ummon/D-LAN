# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib
QT -= gui
TARGET = Tests
CONFIG += link_prl \
    console
CONFIG -= app_bundle
CONFIG(debug, debug|release) { 
    DEFINES += DEBUG
    FOLDER = debug
}
else:FOLDER = release
DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER
LIBS += -L../output/$$FOLDER \
    -lFileManager
POST_TARGETDEPS += ../output/$$FOLDER/libFileManager.a
LIBS += -L../../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../../Common/output/$$FOLDER/libCommon.a

# FIXME : Should not be here, all dependies are read from the prl file (see link_prl):
LIBS += -L../../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../../Common/LogManager/output/$$FOLDER/libLogManager.a
LIBS += -L${PROTOBUF}/src/.libs \
    -lprotobuf
INCLUDEPATH += . \
    .. \
    ../../.. \ # For the 'Common' component.
    ${PROTOBUF}/src
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../../Protos/common.pb.cc \
    HashesReceiver.cpp \
    StressTest.cpp
HEADERS += Tests.h \
    ../../../Protos/common.pb.h \
    debug_new.h \
    HashesReceiver.h \
    StressTest.h
