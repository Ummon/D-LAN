
CONFIG(debug, debug|release) {
   QMAKE_CXXFLAGS += -pg
   QMAKE_LFLAGS += -pg
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
}

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER
RCC_DIR = .tmp/$$FOLDER
UI_DIR = .tmp/$$FOLDER
