# ==============================================================================
# QtService (was Libs/qtservice/src/qtservice.pri, which compiled its sources
# directly into each consumer — RemoteCoreController and D-LAN.Core).
# Now built once as a static library.
#
# The 'examples' and 'buildlib' qmake subprojects are third-party samples not
# used by D-LAN and are intentionally not ported.
# ==============================================================================

add_library(qtservice STATIC
   src/qtservice.cpp
   src/qtservice.h
   src/qtservice_p.h
)

if(WIN32)
   target_sources(qtservice PRIVATE src/qtservice_win.cpp)
   target_link_libraries(qtservice PRIVATE user32)
else()
   target_sources(qtservice PRIVATE
      src/qtservice_unix.cpp
      src/qtunixsocket.cpp
      src/qtunixserversocket.cpp
      src/qtunixsocket.h
      src/qtunixserversocket.h
   )
   target_link_libraries(qtservice PUBLIC Qt6::Network)
endif()

target_link_libraries(qtservice PUBLIC Qt6::Core)

target_include_directories(qtservice PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")

if(DLAN_BUILD_TESTS AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
   add_executable(TestsQtServiceUnix tests/QtServiceUnixTests.cpp)
   target_link_libraries(TestsQtServiceUnix PRIVATE qtservice Qt6::Test)
   target_link_options(TestsQtServiceUnix PRIVATE
      "LINKER:--wrap=connect" "LINKER:--wrap=bind" "LINKER:--wrap=listen")
   add_test(NAME TestsQtServiceUnix COMMAND TestsQtServiceUnix)
   set_tests_properties(TestsQtServiceUnix PROPERTIES TIMEOUT 20)
endif()
