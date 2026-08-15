# ==============================================================================
# LogManager unit tests (was TestsLogManager.pro).
# ==============================================================================

add_executable(TestsLogManager
   main.cpp
   Tests.cpp
   Tests.h
)

target_include_directories(TestsLogManager PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsLogManager PRIVATE
   LogManager
   Common
   Qt6::Test
)

add_test(NAME TestsLogManager COMMAND TestsLogManager)
