# ==============================================================================
# RemoteCoreController static library
# (was Common/RemoteCoreController/RemoteCoreController.pro).
# ==============================================================================

add_library(RemoteCoreController STATIC
   priv/Builder.cpp
   priv/LocalBrowseResult.cpp
   priv/SearchResult.cpp
   priv/CoreConnection.cpp
   priv/BrowseResult.cpp
   priv/CoreController.cpp
   priv/Log.cpp
   priv/InternalCoreConnection.cpp
   priv/SendChatMessageResult.cpp

   Builder.h
   ILocalBrowseResult.h
   priv/LocalBrowseResult.h
   priv/SearchResult.h
   priv/CoreConnection.h
   priv/BrowseResult.h
   ISearchResult.h
   IBrowseResult.h
   ICoreConnection.h
   priv/CoreController.h
   priv/Log.h
   Types.h
   priv/InternalCoreConnection.h
   ISendChatMessageResult.h
   priv/SendChatMessageResult.h
   ILocalBrowseQuickAccessResult.h
   priv/LocalBrowseQuickAccessResult.h
   priv/LocalBrowseQuickAccessResult.cpp
)

target_compile_definitions(RemoteCoreController PRIVATE REMOTECORECONTROLLER_LIBRARY)

target_include_directories(RemoteCoreController
   PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}"
)

target_link_libraries(RemoteCoreController PUBLIC
   Common
   LogManager
   qtservice
   dlan_protos
)
