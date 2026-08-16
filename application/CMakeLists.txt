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
option(DLAN_PROFILING   "Build with gprof instrumentation (-pg), was 'CONFIG += prof'" OFF)

# Third-party locations (were hardcoded in blake3.pri / protobuf.pri).
set(DLAN_BLAKE3_ROOT   "C:/BLAKE3-1.8.5/c" CACHE PATH "Root of the BLAKE3 C library (contains blake3.h and lib/)")
set(DLAN_PROTOBUF_ROOT "C:/protobuf"       CACHE PATH "Root of the protobuf installation")

# ------------------------------------------------------------------------------
# Global compiler / Qt configuration (was common.pri).
# ------------------------------------------------------------------------------
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

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
   add_compile_options(-pg -g)
   add_link_options(-pg)
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
# Uses the standalone qt_add_lrelease() signature (Qt >= 6.7).
# ------------------------------------------------------------------------------
find_package(Qt6 QUIET COMPONENTS LinguistTools)
if(Qt6LinguistTools_FOUND AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/translations")
   file(GLOB DLAN_TS_FILES "${CMAKE_CURRENT_SOURCE_DIR}/translations/*.ts")
   if(DLAN_TS_FILES)
      qt_add_lrelease(
         TS_FILES ${DLAN_TS_FILES}
         QM_FILES_OUTPUT_VARIABLE DLAN_QM_FILES
      )
      add_custom_target(dlan_translations ALL DEPENDS ${DLAN_QM_FILES})
   endif()
endif()
