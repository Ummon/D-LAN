# ==============================================================================
# D-LAN.Client executable (was Client/Client.pro). Console application.
# ==============================================================================

add_executable(DLanClient
   main.cpp
   D-LAN_Client.cpp
   CoreConnectionProxy.cpp
   Log.cpp

   D-LAN_Client.h
   CoreConnectionProxy.h
   Log.h
)

set_target_properties(DLanClient PROPERTIES OUTPUT_NAME "D-LAN.Client")

target_include_directories(DLanClient PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(DLanClient PRIVATE
   RemoteCoreController
   LogManager
   Common
   dlan_protos
)

if(WIN32)
   target_sources(DLanClient PRIVATE "${CMAKE_SOURCE_DIR}/Common/version.rc")
endif()
