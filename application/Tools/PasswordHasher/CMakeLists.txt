# ==============================================================================
# PasswordHasher tool (was Tools/PasswordHasher/PasswordHasher.pro).
# Widgets application.
# ==============================================================================

add_executable(PasswordHasher WIN32
   main.cpp
   MainWindow.cpp
   MainWindow.h
   MainWindow.ui
   resources.qrc
)

set_target_properties(PasswordHasher PROPERTIES
   AUTOUIC ON
   AUTORCC ON
)

target_include_directories(PasswordHasher PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(PasswordHasher PRIVATE
   Common
   dlan_protos
   Qt6::Widgets
)

if(WIN32)
   target_sources(PasswordHasher PRIVATE passwordhasher.rc)
endif()
