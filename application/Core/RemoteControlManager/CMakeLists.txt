# ==============================================================================
# RemoteControlManager static library
# (was Core/RemoteControlManager/RemoteControlManager.pro).
# ==============================================================================

add_library(RemoteControlManager STATIC
   priv/RemoteControlManager.cpp
   priv/RemoteConnection.cpp
   priv/Builder.cpp
   priv/Log.cpp

   IRemoteControlManager.h
   priv/RemoteControlManager.h
   priv/RemoteConnection.h
   Builder.h
   priv/Log.h
)

target_compile_definitions(RemoteControlManager PRIVATE REMOTECONTROLMANAGER_LIBRARY)

target_include_directories(RemoteControlManager PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(RemoteControlManager PUBLIC
   Common
   LogManager
   dlan_protos
)
