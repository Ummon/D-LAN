# -------------------------------------------------
# Project created by QtCreator 2010-05-01T18:27:07
# -------------------------------------------------
TARGET = LogViewer
TEMPLATE = app
CONFIG += link_prl
CONFIG(debug, debug|release) { 
    FOLDER = debug
    DEFINES += DEBUG
}
else:FOLDER = release
DESTDIR = output/$$FOLDER
MOC_DIR = ".tmp/$$FOLDER"
OBJECTS_DIR = ".tmp/$$FOLDER"
RCC_DIR = ".tmp/$$FOLDER"
UI_DIR = ".tmp/$$FOLDER"
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
