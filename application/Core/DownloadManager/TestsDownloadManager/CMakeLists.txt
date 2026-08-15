# ==============================================================================
# DownloadManager tests (was TestsDownloadManager.pro).
# ==============================================================================

add_executable(TestsDownloadManager
   main.cpp
   Tests.cpp
   MockFileManager.cpp
   MockPeerManager.cpp

   Tests.h
   MockFileManager.h
   MockPeerManager.h
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   target_compile_options(TestsDownloadManager PRIVATE -Wno-pessimizing-move -Wno-unused-result)
endif()

target_include_directories(TestsDownloadManager PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
   "${CMAKE_CURRENT_SOURCE_DIR}/../.."
)

target_link_libraries(TestsDownloadManager PRIVATE
   DownloadManager
   FileManager
   PeerManager
   Common
   LogManager
   Qt6::Test
)

add_test(NAME TestsDownloadManager COMMAND TestsDownloadManager)
