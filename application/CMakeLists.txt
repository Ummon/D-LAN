# ==============================================================================
# D-LAN — top-level CMake project.
# Replaces the qmake subdirs projects (Client.pro, Core.pro, GUI.pro) and the
# shared configuration previously spread across Common/common.pri,
# Libs/blake3.pri and Libs/protobuf.pri.
#
# Typical configure (llvm-mingw, Ninja):
#   cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
# ==============================================================================

cmake_minimum_required(VERSION 3.21)

project(D-LAN LANGUAGES CXX)

if(WIN32)
   enable_language(RC) # For version.rc / passwordhasher.rc.
endif()

# ------------------------------------------------------------------------------
# Options.
# ------------------------------------------------------------------------------
option(DLAN_BUILD_TESTS "Build the unit/stress test executables" ON)
option(DLAN_BUILD_TOOLS "Build the tools (LogViewer, FileIndexer, PasswordHasher)" ON)
option(DLAN_PROFILING   "Build with gprof instrumentation (-pg); requires a compatible compiler and runtime" OFF)
option(DLAN_PDB         "Emit CodeView debug info and a .pdb so LM::CrashHandler can print function names and line numbers" ON)

# Third-party locations (were hardcoded in blake3.pri / protobuf.pri).
set(DLAN_BLAKE3_ROOT   "C:/BLAKE3-1.8.7/c" CACHE PATH "Root of the BLAKE3 C library (contains blake3.h and lib/)")
set(DLAN_PROTOBUF_ROOT "C:/protobuf-36.1" CACHE PATH "Root of the protobuf installation")

# ------------------------------------------------------------------------------
# Global compiler / Qt configuration (was common.pri).
# ------------------------------------------------------------------------------
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
   # Keep all targets, including static libraries, position-independent. LTO
   # also needs -fPIC at link time to avoid copies of Qt globals such as
   # QCoreApplication::self, which can cause a crash during GUI startup.
   set(CMAKE_POSITION_INDEPENDENT_CODE ON)
   add_link_options(-fPIC)
endif()

set(CMAKE_AUTOMOC ON)

set(CMAKE_BUILD_PARALLEL_LEVEL 32)

# All executables end up in one place, similar to DESTDIR = output/$$FOLDER.
# (Multi-config generators automatically append the configuration name.)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/output")

add_compile_definitions(
   QT_USE_QSTRINGBUILDER
   $<$<CONFIG:Debug>:DEBUG>
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   # llvm-mingw toolchain: libc++ + the warning set from common.pri.
   add_compile_options(-stdlib=libc++ -Wall -Wno-parentheses -Wno-ignored-attributes)
   add_link_options(-stdlib=libc++)
endif()

# 'CONFIG += ltcg' in release.
include(CheckIPOSupported)
check_ipo_supported(RESULT DLAN_IPO_SUPPORTED OUTPUT _dlan_ipo_msg)
if(DLAN_IPO_SUPPORTED)
   set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
endif()

if(DLAN_PROFILING)
   # Accepting -pg at compile time is not enough: LLVM-MinGW can compile it
   # but fails to link the gprof runtime (unresolved mcount / etext).
   # Keep probe settings local, and force a link even for toolchains that
   # normally use static libraries for try_compile checks.
   function(dlan_check_gprof)
      include(CheckCXXCompilerFlag)
      set(CMAKE_TRY_COMPILE_TARGET_TYPE EXECUTABLE)
      set(CMAKE_REQUIRED_LINK_OPTIONS -pg)
      check_cxx_compiler_flag(-pg DLAN_GPROF_SUPPORTED)
   endfunction()
   dlan_check_gprof()
   if(NOT DLAN_GPROF_SUPPORTED)
      message(FATAL_ERROR
         "DLAN_PROFILING requires a working gprof (-pg) compiler and runtime. "
         "The selected toolchain cannot compile and link a program with -pg "
         "(LLVM-MinGW can fail with unresolved mcount / etext). "
         "Set DLAN_PROFILING=OFF. For Windows CPU sampling, build Release with "
         "DLAN_PDB=ON and use a sampling profiler with the generated .pdb files. "
         "For gprof instrumentation, use a compatible toolchain and dependencies.")
   endif()
   add_compile_options(-pg -g)
   add_link_options(-pg)
endif()

# ------------------------------------------------------------------------------
# Debug information for the crash handler (Common/LogManager/CrashHandler.h).
#
# DbgHelp ('StackWalk64' / 'SymFromAddr') only understands CodeView/PDB, not the DWARF
# llvm-mingw emits by default. Without this, crash reports degrade gracefully to
# 'module+0xRVA' lines, which can still be resolved after the fact with:
#    llvm-addr2line -e output/D-LAN.Core.exe -f -C -i 0x<RVA>
#
# '-gcodeview' costs nothing at runtime and the .pdb is a separate file, so it is kept
# on in release builds too. Configure with -DDLAN_PDB=OFF to opt out.
#
# IMPORTANT: '-gcodeview' *replaces* DWARF, it does not complement it. GDB reads only
# DWARF, so applying it to Debug builds leaves the debugger with no line table and
# breakpoints silently fail to bind (they vanish as soon as the process starts).
# CodeView is therefore restricted to the non-Debug configurations, which are the ones
# where CrashHandler actually has to symbolize a stack in the field.
# ------------------------------------------------------------------------------
if(DLAN_PDB AND WIN32 AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   add_compile_options($<$<NOT:$<CONFIG:Debug>>:-gcodeview> -g)
   # An empty '--pdb=' tells lld to derive the .pdb name from the output binary.
   add_link_options($<$<NOT:$<CONFIG:Debug>>:-Wl,--pdb=> -g)
   # WPA treats lld's forward-slash PDB path as a single filename, which can
   # prevent symbol lookup even when the matching PDB is on the symbol path.
   # Embed only the filename; keep the actual PDB beside the executable.
   add_link_options("$<$<NOT:$<CONFIG:Debug>>:-Wl,--Xlink=/pdbaltpath:$<TARGET_FILE_BASE_NAME:$<TARGET_PROPERTY:NAME>>.pdb>")
endif()

# ------------------------------------------------------------------------------
# Qt.
# ------------------------------------------------------------------------------
list(APPEND CMAKE_PREFIX_PATH "${DLAN_PROTOBUF_ROOT}")

find_package(Qt6 REQUIRED COMPONENTS Core Network Xml Sql Widgets SvgWidgets)

if(DLAN_BUILD_TESTS)
   find_package(Qt6 REQUIRED COMPONENTS Test)
   enable_testing()
endif()

# ------------------------------------------------------------------------------
# BLAKE3 (was Libs/blake3.pri).
# ------------------------------------------------------------------------------
find_library(DLAN_BLAKE3_LIBRARY
   NAMES blake3
   PATHS "${DLAN_BLAKE3_ROOT}/lib"
)
find_path(DLAN_BLAKE3_INCLUDE_DIR
   NAMES blake3.h
   PATHS "${DLAN_BLAKE3_ROOT}"
)
if(NOT DLAN_BLAKE3_LIBRARY OR NOT DLAN_BLAKE3_INCLUDE_DIR)
   message(FATAL_ERROR "BLAKE3 not found. Set DLAN_BLAKE3_ROOT (currently '${DLAN_BLAKE3_ROOT}').")
endif()
add_library(blake3 UNKNOWN IMPORTED)
set_target_properties(blake3 PROPERTIES
   IMPORTED_LOCATION "${DLAN_BLAKE3_LIBRARY}"
   INTERFACE_INCLUDE_DIRECTORIES "${DLAN_BLAKE3_INCLUDE_DIR}"
)

# ------------------------------------------------------------------------------
# Protobuf (was Libs/protobuf.pri).
# Prefer the CMake package config shipped by protobuf itself; fall back to
# pkg-config (the mechanism the qmake build used).
# ------------------------------------------------------------------------------
find_package(protobuf CONFIG QUIET)
if(protobuf_FOUND)
   set(DLAN_PROTOBUF_TARGET protobuf::libprotobuf)
else()
   find_package(PkgConfig REQUIRED)
   set(ENV{PKG_CONFIG_PATH} "${DLAN_PROTOBUF_ROOT}/lib/pkgconfig;$ENV{PKG_CONFIG_PATH}")
   # The qmake build passed --define-variable=prefix=... to pkg-config; if the
   # .pc file has a stale prefix, set the PKG_CONFIG environment variable to
   # 'pkg-config --define-variable=prefix=C:/protobuf' before configuring.
   pkg_check_modules(dlan_protobuf REQUIRED IMPORTED_TARGET protobuf)
   set(DLAN_PROTOBUF_TARGET PkgConfig::dlan_protobuf)
endif()

find_program(DLAN_PROTOC protoc
   HINTS "${DLAN_PROTOBUF_ROOT}/bin" "${DLAN_PROTOBUF_ROOT}/build"
   REQUIRED
)

# ------------------------------------------------------------------------------
# Subdirectories.
# ------------------------------------------------------------------------------
add_subdirectory(Protos)
add_subdirectory(Libs/qtservice)

add_subdirectory(Common)
add_subdirectory(Common/LogManager)
add_subdirectory(Common/RemoteCoreController)

add_subdirectory(Core/HashCache)
add_subdirectory(Core/FileManager)
add_subdirectory(Core/PeerManager)
add_subdirectory(Core/UploadManager)
add_subdirectory(Core/DownloadManager)
add_subdirectory(Core/NetworkListener)
add_subdirectory(Core/ChatSystem)
add_subdirectory(Core/RemoteControlManager)

add_subdirectory(Core)   # D-LAN.Core executable.
add_subdirectory(GUI)    # D-LAN.GUI executable.
# add_subdirectory(Client) # D-LAN.Client executable. # Disabled for now.

if(DLAN_BUILD_TOOLS)
   add_subdirectory(Tools/FileIndexer)
   add_subdirectory(Tools/LogViewer)
   add_subdirectory(Tools/PasswordHasher)
endif()

# ------------------------------------------------------------------------------
# Translations (TRANSLATIONS in Core.pro / GUI.pro).
# Uses the standalone qt_add_lupdate() / qt_add_lrelease() signatures (Qt >= 6.7).
#
#  * 'cmake --build <dir> --target update_translations' refreshes the .ts files from
#    the sources, it replaces the 'lupdate-pro Core.pro / GUI.pro' calls of the qmake
#    era. The source files are taken from the targets themselves, so the two sets
#    below only have to mirror the SUBDIRS of the old Core.pro and GUI.pro.
#  * The .qm files are built as part of 'all', in ${CMAKE_BINARY_DIR}.
# ------------------------------------------------------------------------------
find_package(Qt6 QUIET COMPONENTS LinguistTools)
if(Qt6LinguistTools_FOUND AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/translations")
   file(GLOB DLAN_CORE_TS_FILES "${CMAKE_CURRENT_SOURCE_DIR}/translations/d_lan_core.*.ts")
   file(GLOB DLAN_GUI_TS_FILES  "${CMAKE_CURRENT_SOURCE_DIR}/translations/d_lan_gui.*.ts")

   if(DLAN_CORE_TS_FILES)
      qt_add_lupdate(
         SOURCE_TARGETS
            Common
            LogManager
            HashCache
            FileManager
            PeerManager
            UploadManager
            DownloadManager
            NetworkListener
            ChatSystem
            RemoteControlManager
            DLanCore
         TS_FILES ${DLAN_CORE_TS_FILES}
         LUPDATE_TARGET dlan_lupdate_core
      )
   endif()

   if(DLAN_GUI_TS_FILES)
      qt_add_lupdate(
         SOURCE_TARGETS
            Common
            LogManager
            RemoteCoreController
            DLanGUI
         TS_FILES ${DLAN_GUI_TS_FILES}
         LUPDATE_TARGET dlan_lupdate_gui
      )
   endif()

   if(DLAN_CORE_TS_FILES OR DLAN_GUI_TS_FILES)
      qt_add_lrelease(
         TS_FILES ${DLAN_CORE_TS_FILES} ${DLAN_GUI_TS_FILES}
         QM_FILES_OUTPUT_VARIABLE DLAN_QM_FILES
      )
      add_custom_target(dlan_translations ALL DEPENDS ${DLAN_QM_FILES})
   endif()
endif()
