# ==============================================================================
# NetworkListener tests (was TestsNetworkListener.pro).
# ==============================================================================

add_executable(TestsNetworkListener
   main.cpp
   Tests.cpp

   Tests.h
   MockHashCache.h
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   target_compile_options(TestsNetworkListener PRIVATE -Wno-pessimizing-move -Wno-unused-result)
endif()

target_include_directories(TestsNetworkListener PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsNetworkListener PRIVATE
   NetworkListener
   DownloadManager
   UploadManager
   PeerManager
   FileManager
   LogManager
   Common
   Qt6::Test
)

add_test(NAME TestsNetworkListener COMMAND TestsNetworkListener)
