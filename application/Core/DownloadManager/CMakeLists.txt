# ==============================================================================
# DownloadManager static library (was Core/DownloadManager/DownloadManager.pro).
# ==============================================================================

add_library(DownloadManager STATIC
   priv/FileDownload.cpp
   priv/DownloadManager.cpp
   priv/Download.cpp
   priv/DirDownload.cpp
   priv/Builder.cpp
   priv/OccupiedPeers.cpp
   priv/Log.cpp
   priv/DownloadPredicate.cpp
   priv/DownloadQueue.cpp
   priv/ChunkDownloader.cpp
   priv/Utils.cpp

   IDownloadManager.h
   IDownload.h
   IChunkDownloader.h
   priv/FileDownload.h
   priv/DownloadManager.h
   priv/Download.h
   priv/DirDownload.h
   Builder.h
   priv/Constants.h
   priv/OccupiedPeers.h
   priv/Log.h
   priv/DownloadPredicate.h
   priv/DownloadQueue.h
   Utils.h
   priv/LinkedPeers.h
   priv/ChunkDownloader.h
)

target_compile_definitions(DownloadManager PRIVATE DOWNLOADMANAGER_LIBRARY)

target_include_directories(DownloadManager PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(DownloadManager PUBLIC
   Common
   LogManager
   dlan_protos
)

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsDownloadManager)
endif()
