QT += core network
TARGET = "D-LAN.Client"
TEMPLATE = app
CONFIG += console

RC_FILE = ../Common/version.rc

include(../Common/common.pri)
include(../Libs/protobuf.pri)

INCLUDEPATH += . ..

LIBS += -L../Common/RemoteCoreController/output/$$FOLDER \
    -lRemoteCoreController
PRE_TARGETDEPS += ../Common/RemoteCoreController/output/$$FOLDER/libRemoteCoreController.a

LIBS += -L../Common/LogManager/output/$$FOLDER \
    -lLogManager
PRE_TARGETDEPS += ../Common/LogManager/output/$$FOLDER/libLogManager.a

LIBS += -L../Common/output/$$FOLDER \
    -lCommon
PRE_TARGETDEPS += ../Common/output/$$FOLDER/libCommon.a

SOURCES += D-LAN_Client.cpp \
    ../Protos/gui_protocol.pb.cc \
    ../Protos/common.pb.cc \
    ../Protos/gui_settings.pb.cc \
    ../Protos/core_settings.pb.cc \
    main.cpp \
    CoreConnectionProxy.cpp \
    Log.cpp

HEADERS  += D-LAN_Client.h \
    ../Protos/gui_protocol.pb.h \
    ../Protos/common.pb.h \
    ../Protos/gui_settings.pb.h \
    ../Protos/core_settings.pb.h \
    CoreConnectionProxy.h \
    Log.h
