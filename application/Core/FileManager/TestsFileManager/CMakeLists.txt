# ==============================================================================
# FileManager tests (was TestsFileManager.pro).
# ==============================================================================

add_executable(TestsFileManager
   main.cpp
   CacheTest.cpp
   MockHashCache.cpp
   Tests.cpp
   HashesReceiver.cpp
   StressTest.cpp
   StressTests.cpp
   Utils.cpp
   WordIndexTests.cpp

   Tests.h
   CacheTest.h
   HashesReceiver.h
   MockHashCache.h
   StressTest.h
   StressTests.h
   Utils.h
   WordIndexTests.h
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   target_compile_options(TestsFileManager PRIVATE -Wno-pessimizing-move -Wno-unused-result)
endif()

target_include_directories(TestsFileManager PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsFileManager PRIVATE
   FileManager
   Common
   LogManager
   Qt6::Test
)

add_test(NAME TestsFileManager COMMAND TestsFileManager)
