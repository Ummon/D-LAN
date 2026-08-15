# ==============================================================================
# D-LAN.Core executable (was Core/Core.pro).
#
# The NVWA leak detector (DEFINES += ENABLE_NVWA) was not ported; add
# ../Libs/Nvwa/debug_new.cpp and the defines manually if you need it again.
# ==============================================================================

add_executable(DLanCore
   main.cpp
   Log.cpp
   CoreService.cpp
   Core.cpp
   CoreApplication.cpp

   Log.h
   CoreService.h
   Core.h
   CoreApplication.h
)

set_target_properties(DLanCore PROPERTIES OUTPUT_NAME "D-LAN.Core")

target_include_directories(DLanCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(DLanCore PRIVATE
   RemoteControlManager
   ChatSystem
   NetworkListener
   DownloadManager
   UploadManager
   PeerManager
   FileManager
   HashCache
   LogManager
   Common
   qtservice
   dlan_protos
)

if(WIN32)
   target_sources(DLanCore PRIVATE "${CMAKE_SOURCE_DIR}/Common/version.rc")
   # Used by UDPListener::initMulticastUDPSocket().
   target_link_libraries(DLanCore PRIVATE wsock32)
endif()
