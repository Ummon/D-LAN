# ==============================================================================
# PeerManager tests (was TestsPeerManager.pro).
# ==============================================================================

add_executable(TestsPeerManager
   main.cpp
   MockHashCache.cpp
   Tests.cpp
   TestServer.cpp
   PeerUpdater.cpp
   ResultListener.cpp

   Tests.h
   MockHashCache.h
   TestServer.h
   PeerUpdater.h
   ResultListener.h
)

target_include_directories(TestsPeerManager PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsPeerManager PRIVATE
   PeerManager
   FileManager
   Common
   LogManager
   Qt6::Test
)

add_test(NAME TestsPeerManager COMMAND TestsPeerManager)
