# ==============================================================================
# ProtoBinReader tool (was Tools/ProtoBinReader/ProtoBinReader.pro).
# Console app; reads serialized queue/files_cache/common messages.
# ==============================================================================

add_executable(ProtoBinReader
   main.cpp
)

target_include_directories(ProtoBinReader PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_SOURCE_DIR}"
)

target_link_libraries(ProtoBinReader PRIVATE
   dlan_protos
   Qt6::Core
)
