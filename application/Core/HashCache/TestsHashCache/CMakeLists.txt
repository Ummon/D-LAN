# ==============================================================================
# HashCache tests.
#
# NOTE: the original TestsHashCache.pro looked like a stale copy of
# TestsFileManager.pro (its TARGET was 'TestsFileManager' and it linked
# FileManager instead of HashCache). This CMake version keeps the source list
# from the .pro but fixes the target name and links HashCache as well.
# Verify the source list against the actual directory contents.
# ==============================================================================

add_executable(TestsHashCache
   main.cpp
   Tests.cpp

   Tests.h
)

target_include_directories(TestsHashCache PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsHashCache PRIVATE
   HashCache
   FileManager
   Common
   LogManager
   Qt6::Test
)

add_test(NAME TestsHashCache COMMAND TestsHashCache)
