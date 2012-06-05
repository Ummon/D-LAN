CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
   prof {
      QMAKE_CXXFLAGS += -pg -g
      QMAKE_LFLAGS += -pg
      QMAKE_LFLAGS_RELEASE -= -Wl,-s
   }
}

# C++11
win32 {
   # For mingw32.
   QMAKE_CXXFLAGS += -std=gnu++0x
}
macx {
   # For clang.
   QMAKE_CXXFLAGS += -std=c++11 -stdlib=libc++
}
unix {
   QMAKE_CXXFLAGS += -std=c++0x
}

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER
RCC_DIR = .tmp/$$FOLDER
UI_DIR = .tmp/$$FOLDER

QMAKE_CXXFLAGS_WARN_ON = -Wall -Wno-parentheses
