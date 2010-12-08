# -------------------------------------------------
# Project created by QtCreator 2010-05-01T18:27:07
# -------------------------------------------------
TARGET = LogViewer
TEMPLATE = app
CONFIG += link_prl

include(../../Common/common.pri)

INCLUDEPATH += . \
    ../..
LIBS += -L../../Common/LogManager/output/$$FOLDER \
    -lLogManager
POST_TARGETDEPS += ../../Common/LogManager/output/$$FOLDER/libLogManager.a
LIBS += -L../../Common/output/$$FOLDER \
    -lCommon
POST_TARGETDEPS += ../../Common/output/$$FOLDER/libCommon.a
SOURCES += main.cpp \
    MainWindow.cpp \
    TableLogModel.cpp \
    TableLogItemDelegate.cpp \
    TooglableList/TooglableList.cpp \
    TooglableList/TooglableListButton.cpp
HEADERS += MainWindow.h \
    TableLogModel.h \
    TableLogItemDelegate.h \
    TooglableList/TooglableList.h \
    TooglableList/TooglableListButton.h
FORMS += MainWindow.ui \
    TooglableList.ui
RESOURCES += resources.qrc
