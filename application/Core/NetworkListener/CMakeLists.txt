# ==============================================================================
# NetworkListener static library (was Core/NetworkListener/NetworkListener.pro).
# ==============================================================================

add_library(NetworkListener STATIC
   priv/UDPListener.cpp
   priv/TCPListener.cpp
   priv/Search.cpp
   priv/NetworkListener.cpp
   priv/Builder.cpp
   priv/Log.cpp
   priv/Utils.cpp

   ISearch.h
   INetworkListener.h
   priv/UDPListener.h
   priv/TCPListener.h
   priv/Search.h
   priv/NetworkListener.h
   Builder.h
   priv/Log.h
   priv/Utils.h
)

target_compile_definitions(NetworkListener PRIVATE NETWORKLISTENER_LIBRARY)

target_include_directories(NetworkListener PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(NetworkListener PUBLIC
   Common
   LogManager
   dlan_protos
)

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsNetworkListener)
endif()
