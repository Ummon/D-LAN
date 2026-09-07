# ==============================================================================
# FileManager static library (was Core/FileManager/FileManager.pro).
# ==============================================================================

add_library(FileManager STATIC
   priv/Builder.cpp
   priv/FileManager.cpp
   priv/FileUpdater/FileUpdater.cpp
   priv/FileUpdater/DirWatcher.cpp
   priv/Cache/Entry.cpp
   priv/Cache/File.cpp
   priv/Cache/Directory.cpp
   priv/ChunkIndex/Chunks.cpp
   priv/Cache/Chunk.cpp
   priv/Cache/DataReader.cpp
   priv/Cache/DataWriter.cpp
   priv/Cache/Cache.cpp
   priv/FileUpdater/WaitCondition.cpp
   priv/GetHashesResult.cpp
   priv/Log.cpp
   priv/Global.cpp
   priv/Cache/FilePool.cpp
   priv/Cache/FileHasher.cpp
   priv/GetEntriesResult.cpp
   priv/Cache/SharedEntry.cpp
   priv/SizeIndex.cpp

   IGetHashesResult.h
   IFileManager.h
   IChunk.h
   Builder.h
   priv/Log.h
   priv/FileManager.h
   priv/FileUpdater/FileUpdater.h
   priv/FileUpdater/HashingQueue.h
   priv/FileUpdater/DirWatcher.h
   priv/Cache/Entry.h
   priv/Cache/File.h
   priv/Cache/Directory.h
   priv/ChunkIndex/Chunks.h
   priv/SizeIndex.h
   priv/WordIndex/WordIndex.h
   priv/WordIndex/Node.h
   IDataReader.h
   IDataWriter.h
   priv/Cache/Chunk.h
   priv/Cache/DataReader.h
   priv/Cache/DataWriter.h
   priv/Cache/Cache.h
   priv/Exceptions.h
   Exceptions.h
   priv/FileUpdater/WaitCondition.h
   priv/Constants.h
   priv/GetHashesResult.h
   priv/Global.h
   priv/Cache/FilePool.h
   priv/Cache/FileHasher.h
   IGetEntriesResult.h
   priv/GetEntriesResult.h
   priv/ExtensionIndex.h
   priv/Cache/SharedEntry.h
)

if(WIN32)
   target_sources(FileManager PRIVATE
      priv/Cache/FilePoolWin.cpp
      priv/FileUpdater/WaitConditionWin.cpp
      priv/FileUpdater/DirWatcherWin.cpp
      priv/FileUpdater/WaitConditionWin.h
      priv/FileUpdater/DirWatcherWin.h
   )
elseif(APPLE)
   target_sources(FileManager PRIVATE
      priv/FileUpdater/WaitConditionDarwin.cpp
      priv/FileUpdater/WaitConditionDarwin.h
   )
elseif(UNIX)
   target_sources(FileManager PRIVATE
      priv/FileUpdater/WaitConditionLinux.cpp
      priv/FileUpdater/DirWatcherLinux.cpp
      priv/FileUpdater/WaitConditionLinux.h
      priv/FileUpdater/DirWatcherLinux.h
   )
endif()

if(NOT WIN32)
   target_sources(FileManager PRIVATE priv/Cache/FilePoolQt.cpp)
endif()

target_compile_definitions(FileManager PRIVATE FILEMANAGER_LIBRARY)

target_include_directories(FileManager PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(FileManager PUBLIC
   Common
   LogManager
   dlan_protos
)

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsFileManager)
endif()
