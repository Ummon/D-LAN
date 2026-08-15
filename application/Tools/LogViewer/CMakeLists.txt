# ==============================================================================
# LogViewer tool (was Tools/LogViewer/LogViewer.pro). Widgets application.
#
# The .ui files are located via AUTOUIC_SEARCH_PATHS because the original
# FORMS entry 'TooglableList.ui' did not specify a directory.
# ==============================================================================

add_executable(LogViewer WIN32
   main.cpp
   Log.cpp
   MainWindow.cpp
   TableLogModel.cpp
   TableLogItemDelegate.cpp
   TooglableList/TooglableList.cpp
   TooglableList/TooglableListButton.cpp

   MainWindow.h
   Log.h
   TableLogModel.h
   TableLogItemDelegate.h
   TooglableList/TooglableList.h
   TooglableList/TooglableListButton.h

   resources.qrc
)

set_target_properties(LogViewer PROPERTIES
   AUTOUIC ON
   AUTORCC ON
   AUTOUIC_SEARCH_PATHS "${CMAKE_CURRENT_SOURCE_DIR};${CMAKE_CURRENT_SOURCE_DIR}/TooglableList"
)

target_include_directories(LogViewer PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(LogViewer PRIVATE
   LogManager
   Common
   Qt6::Widgets
)
