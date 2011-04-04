QT -= gui
QT += network
TARGET = RemoteCoreController

INCLUDEPATH += . \
   .. \
   ../..
TEMPLATE = lib
CONFIG += staticlib create_prl

include(../common.pri)
include(../../Libs/protobuf.pri)
include(../../Libs/qtservice/src/qtservice.pri)

DEFINES += REMOTECORECONTROLLER_LIBRARY

SOURCES += priv/Builder.cpp \
    priv/SearchResult.cpp \
    priv/CoreConnection.cpp \
    priv/BrowseResult.cpp \
    ../../Protos/common.pb.cc \
    priv/CoreController.cpp \
    ../../Protos/gui_protocol.pb.cc \
    priv/Log.cpp
HEADERS += \
    Builder.h \
    priv/SearchResult.h \
    priv/CoreConnection.h \
    priv/BrowseResult.h \
    ISearchResult.h \
    IBrowseResult.h \
    ICoreConnection.h \
    ../../Protos/common.pb.h \
    priv/CoreController.h \
    ../../Protos/gui_protocol.pb.h \
    priv/Log.h \
    Types.h
