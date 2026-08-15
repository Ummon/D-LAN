# ==============================================================================
# LogManager static library (was Common/LogManager/LogManager.pro).
# ==============================================================================

add_library(LogManager STATIC
   priv/Logger.cpp
   priv/Entry.cpp
   priv/Builder.cpp
   priv/QtLogger.cpp
   priv/StdLogger.cpp
   priv/LoggerHook.cpp

   ILogger.h
   ILoggable.h
   IEntry.h
   Exceptions.h
   Builder.h
   priv/Logger.h
   priv/Entry.h
   priv/QtLogger.h
   priv/StdLogger.h
   priv/LoggerHook.h
   ILoggerHook.h
   LogMacros.h
   Constants.h
)

target_compile_definitions(LogManager PRIVATE LOGMANAGER_LIBRARY)

target_include_directories(LogManager PUBLIC
   "${CMAKE_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}"
)

target_link_libraries(LogManager PUBLIC Qt6::Core)

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsLogManager)
endif()
