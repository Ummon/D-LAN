CONFIG(debug, debug|release) {
   FOLDER = debug
   DEFINES += DEBUG
} else {
   FOLDER = release
   # Disable, GCC 4.6 exit with an error during link time of 'TestsFileManager.exe' when using '1.regen.all.sh'.
   # QMAKE_CXXFLAGS_RELEASE += -flto
   # QMAKE_LFLAGS_RELEASE += -flto -Wl,-allow-multiple-definition
   prof {
      QMAKE_CXXFLAGS += -pg -g
      QMAKE_LFLAGS += -pg
      QMAKE_LFLAGS_RELEASE -= -Wl,-s
   }
}

# C++11
win32 {
   # For mingw32.
   QMAKE_CXXFLAGS += -std=c++11
}
macx {
   # For clang.
   QMAKE_CXXFLAGS += -std=c++11 -stdlib=libc++
}
linux-g++ {
   QMAKE_CXXFLAGS += -std=c++11
}

CONFIG += exceptions rtti

DESTDIR = output/$$FOLDER
MOC_DIR = .tmp/$$FOLDER
OBJECTS_DIR = .tmp/$$FOLDER
RCC_DIR = .tmp/$$FOLDER
UI_DIR = .tmp/$$FOLDER

QMAKE_CXXFLAGS_WARN_ON = -Wall -Wno-parentheses
