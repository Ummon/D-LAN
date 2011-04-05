# -------------------------------------------------
# Project created by QtCreator 2009-10-04T02:24:09
# -------------------------------------------------
QT += testlib network
QT -= gui
TARGET = TestsCore
CONFIG += link_prl console
CONFIG -= app_bundle

include(../../Libs/protobuf.pri)
include(../../Common/common.pri)

LIBS += -L../../Common/RemoteCoreController/output/$$FOLDER \
    -lRemoteCoreController
POST_TARGETDEPS += ../../Common/RemoteCoreController/output/$$FOLDER/libRemoteCoreController.a

LIBS += -L../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a

INCLUDEPATH += . \
    .. \
    ../.. # For the 'Common' component.
TEMPLATE = app
SOURCES += main.cpp \
    Tests.cpp \
    ../../Protos/core_settings.pb.cc \
    ../Log.cpp \
    ../Core.cpp
HEADERS += Tests.h \
    ../../Protos/core_settings.pb.h \
    ../Log.h \
    ../Core.h

OTHER_FILES +=
