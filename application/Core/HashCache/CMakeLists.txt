# ==============================================================================
# HashCache static library (was Core/HashCache/HashCache.pro).
# ==============================================================================

add_library(HashCache STATIC
   priv/Log.cpp
   priv/Builder.cpp
   priv/HashCache.cpp

   IHashCache.h
   Builder.h
   priv/Constants.h
   priv/Exceptions.h
   priv/Log.h
   priv/HashCache.h
)

target_compile_definitions(HashCache PRIVATE HASHCACHE_LIBRARY)

target_include_directories(HashCache PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(HashCache PUBLIC
   Common
   LogManager
   dlan_protos
   Qt6::Sql
)

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsHashCache)
endif()
