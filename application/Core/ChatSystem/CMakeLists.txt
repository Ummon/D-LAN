# ==============================================================================
# ChatSystem static library (was Core/ChatSystem/ChatSystem.pro).
# ==============================================================================

add_library(ChatSystem STATIC
   Builder.cpp
   priv/Log.cpp
   priv/ChatSystem.cpp
   priv/ChatMessage.cpp
   priv/ChatMessages.cpp

   IChatSystem.h
   priv/ChatSystem.h
   Builder.h
   priv/Log.h
   priv/ChatMessage.h
   priv/ChatMessages.h
)

target_compile_definitions(ChatSystem PRIVATE CHATSYSTEM_LIBRARY)

target_include_directories(ChatSystem PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(ChatSystem PUBLIC
   Common
   LogManager
)
