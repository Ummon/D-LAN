# ==============================================================================
# UploadManager static library (was Core/UploadManager/UploadManager.pro).
# ==============================================================================

add_library(UploadManager STATIC
   priv/UploadManager.cpp
   priv/ChunksUploader.cpp
   priv/Log.cpp
   priv/Builder.cpp

   IUploadManager.h
   IChunksUploader.h
   priv/ChunksUploader.h
   priv/UploadManager.h
   Builder.h
   priv/Log.h
)

target_compile_definitions(UploadManager PRIVATE UPLOADMANAGER_LIBRARY)

target_include_directories(UploadManager PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(UploadManager PUBLIC
   Common
   LogManager
   dlan_protos
)
