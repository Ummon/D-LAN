# ==============================================================================
# Common unit tests (was Common/TestsCommon/TestsCommon.pro).
# ==============================================================================

add_executable(TestsCommon
   main.cpp
   Tests.cpp
   TreeTests.cpp
   BenchmarkTests.cpp

   Tests.h
   TreeTests.h
   BenchmarkTests.h
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   target_compile_options(TestsCommon PRIVATE -Wno-pessimizing-move -Wno-unused-result)
endif()

target_include_directories(TestsCommon PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsCommon PRIVATE
   Common
   LogManager
   Qt6::Test
)

add_test(NAME TestsCommon COMMAND TestsCommon)
