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
   # A systemd unit on Linux, no service on the other systems.
   target_sources(qtservice PRIVATE src/qtservice_unix.cpp)
endif()

target_link_libraries(qtservice PUBLIC Qt6::Core)

target_include_directories(qtservice PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")
