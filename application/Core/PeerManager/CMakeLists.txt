# ==============================================================================
# PeerManager static library (was Core/PeerManager/PeerManager.pro).
# ==============================================================================

add_library(PeerManager STATIC
   priv/PeerManager.cpp
   priv/GetChunksResult.cpp
   priv/Peer.cpp
   priv/Builder.cpp
   priv/ConnectionPool.cpp
   priv/GetEntriesResult.cpp
   priv/GetHashesResult.cpp
   priv/Log.cpp
   priv/PeerSelf.cpp
   priv/PeerMessageSocket.cpp

   IPeerManager.h
   IGetChunksResult.h
   IPeer.h
   priv/GetChunksResult.h
   priv/PeerManager.h
   priv/Peer.h
   Builder.h
   priv/Log.h
   priv/Constants.h
   priv/ConnectionPool.h
   priv/PeerMessageSocket.h
   IGetEntriesResult.h
   IGetHashesResult.h
   ISocket.h
   priv/GetEntriesResult.h
   priv/GetHashesResult.h
   priv/PeerSelf.h
)

target_compile_definitions(PeerManager PRIVATE PEERMANAGER_LIBRARY)

target_include_directories(PeerManager PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(PeerManager PUBLIC
   Common
   LogManager
   dlan_protos
)

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsPeerManager)
endif()
