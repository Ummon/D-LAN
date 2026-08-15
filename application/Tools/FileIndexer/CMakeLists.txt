# ==============================================================================
# FileIndexer tool (was Tools/FileIndexer/FileIndexer.pro). Console app.
# ==============================================================================

add_executable(FileIndexer
   main.cpp
   OldWordIndex.h
   OldNode.h
)

target_include_directories(FileIndexer PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_SOURCE_DIR}/Core/FileManager"
)

target_link_libraries(FileIndexer PRIVATE Common)
