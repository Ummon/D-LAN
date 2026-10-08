# ==============================================================================
# QtService (was Libs/qtservice/src/qtservice.pri, which compiled its sources
# directly into each consumer — RemoteCoreController and D-LAN.Core).
# Now built once as a static library.
#
# Only what D-LAN uses has been kept from the original Qt Solutions component (see README.TXT).
# ==============================================================================

add_library(qtservice STATIC
   src/qtservice.cpp
   src/qtservice.h
   src/qtservice_p.h
)

if(WIN32)
   target_sources(qtservice PRIVATE src/qtservice_win.cpp)
else()
   # A systemd unit on Linux, no service on the other systems.
   target_sources(qtservice PRIVATE src/qtservice_unix.cpp)
endif()

target_link_libraries(qtservice PUBLIC Qt6::Core)

target_include_directories(qtservice PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/src")
