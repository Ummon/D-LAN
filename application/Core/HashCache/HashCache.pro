QT -= gui
TARGET = HashCache
TEMPLATE = lib

include(../../Common/common.pri)
include(../../Libs/protobuf.pri)

CONFIG += staticlib link_prl create_prl

INCLUDEPATH += . ../..

DEFINES += HASHCACHE_LIBRARY

SOURCES += ../../Protos/common.pb.cc \
    ../../Protos/hash_cache.pb.cc \
    priv/Log.cpp \
    priv/Builder.cpp \
    priv/HashCache.cpp
HEADERS += IHashCache.h \
    Builder.h \
    priv/Log.h \
    ../../Protos/common.pb.h \
    ../../Protos/hash_cache.pb.h \
    IRootEntry.h \
    IEntry.h \
    IDir.h \
    IFile.h \
    priv/HashCache.h
OTHER_FILES +=
