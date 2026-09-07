# ==============================================================================
# RemoteControlManager static library
# (was Core/RemoteControlManager/RemoteControlManager.pro).
# ==============================================================================

add_library(RemoteControlManager STATIC
   priv/RemoteControlManager.cpp
   priv/RemoteConnection.cpp
   priv/LocalBrowse.cpp
   priv/Builder.cpp
   priv/Log.cpp

   IRemoteControlManager.h
   priv/RemoteControlManager.h
   priv/RemoteConnection.h
   priv/UploadProgress.h
   priv/LocalBrowse.h
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

if(DLAN_BUILD_TESTS)
   add_executable(TestsRemoteControlManager TestsRemoteControlManager/Tests.cpp TestsRemoteControlManager/Mocks.h)
   target_include_directories(TestsRemoteControlManager PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
   target_link_libraries(TestsRemoteControlManager PRIVATE RemoteControlManager PeerManager FileManager NetworkListener ChatSystem Qt6::Test)
   add_test(NAME TestsRemoteControlManager COMMAND TestsRemoteControlManager)
endif()
