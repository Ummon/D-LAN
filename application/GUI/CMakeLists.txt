# ==============================================================================
# D-LAN.GUI executable (was GUI/GUI.pro).
#
# Notes vs the qmake build:
#  - .ui files are listed as sources and processed by AUTOUIC.
#  - resources.qrc is processed by AUTORCC.
#  - QT += winextras had already been dropped in the Qt6 migration.
#  - The NVWA leak detector block was not ported.
# ==============================================================================

add_executable(DLanGUI WIN32
   main.cpp
   Chat/EmoticonTextDocument.cpp
   RemoteBrowseDialog/RemoteBrowseDialog.cpp
   RemoteBrowseDialog/RemoteBrowseModel.cpp
   MainWindow.cpp
   Settings/SharedEntryListDelegate.cpp
   Settings/SharedEntryListModel.cpp
   StatusBar.cpp
   DialogAbout.cpp
   Log.cpp
   CheckBoxList.cpp
   Browse/BrowseModel.cpp
   Chat/ChatModel.cpp
   Downloads/DownloadsFlatModel.cpp
   Log/LogModel.cpp
   Search/SearchModel.cpp
   Uploads/UploadsModel.cpp
   Settings/RemoteFileDialog.cpp
   DownloadMenu.cpp
   D-LAN_GUI.cpp
   ProgressBar.cpp
   IconProvider.cpp
   Utils.cpp
   UtilsLocations.cpp
   Settings/AskNewPasswordDialog.cpp
   Downloads/DownloadsTreeModel.cpp
   Downloads/DownloadsModel.cpp
   BusyIndicator.cpp
   Log/LogDelegate.cpp
   Chat/RoomsModel.cpp
   Chat/RoomsDelegate.cpp
   Peers/PeersDock.cpp
   Peers/PeerListModel.cpp
   Peers/PeerListDelegate.cpp
   Chat/RoomsDock.cpp
   Search/SearchDock.cpp
   MDI/TabButtons.cpp
   MDI/MdiArea.cpp
   Browse/BrowseWidget.cpp
   Chat/ChatWidget.cpp
   Downloads/DownloadsWidget.cpp
   Search/SearchWidget.cpp
   Settings/SettingsWidget.cpp
   Uploads/UploadsWidget.cpp
   MDI/MdiWidget.cpp
   ColorBox.cpp
   Chat/ChatTextEdit.cpp
   Constants.cpp
   Emoticons/EmoticonsWidget.cpp
   Emoticons/Emoticons.cpp
   Emoticons/SingleEmoticonWidget.cpp
   AutoComplete/AutoComplete.cpp
   AutoComplete/AutoCompleteModel.cpp
   Search/SearchUtils.cpp

   MainWindow.h
   Chat/EmoticonTextDocument.h
   RemoteBrowseDialog/RemoteBrowseDialog.h
   RemoteBrowseDialog/RemoteBrowseModel.h
   Settings/SharedEntryListDelegate.h
   Settings/SharedEntryListModel.h
   StatusBar.h
   Log.h
   DialogAbout.h
   CheckBoxList.h
   CheckBoxModel.h
   IFilter.h
   Browse/BrowseModel.h
   Chat/ChatModel.h
   Downloads/DownloadFilterStatus.h
   Log/LogModel.h
   Search/SearchModel.h
   Uploads/UploadsModel.h
   Settings/RemoteFileDialog.h
   DownloadMenu.h
   D-LAN_GUI.h
   ProgressBar.h
   IconProvider.h
   Utils.h
   Settings/AskNewPasswordDialog.h
   Downloads/DownloadsFlatModel.h
   Downloads/DownloadsTreeModel.h
   Downloads/DownloadsModel.h
   BusyIndicator.h
   Taskbar/Taskbar.h
   Taskbar/ITaskbarImpl.h
   Taskbar/TaskbarTypes.h
   Log/LogDelegate.h
   Chat/RoomsModel.h
   Chat/RoomsDelegate.h
   Peers/PeersDock.h
   Peers/PeerListModel.h
   Peers/PeerListDelegate.h
   Chat/RoomsDock.h
   Search/SearchDock.h
   MDI/TabButtons.h
   MDI/MdiArea.h
   MDI/MdiWidget.h
   Browse/BrowseWidget.h
   Chat/ChatWidget.h
   Downloads/DownloadsWidget.h
   Search/SearchWidget.h
   Settings/SettingsWidget.h
   Uploads/UploadsWidget.h
   ColorBox.h
   Chat/ChatTextEdit.h
   Constants.h
   Emoticons/EmoticonsWidget.h
   Emoticons/Emoticons.h
   Emoticons/SingleEmoticonWidget.h
   AutoComplete/AutoComplete.h
   AutoComplete/AutoCompleteModel.h
   Search/SearchUtils.h

   # FORMS
   MainWindow.ui
   RemoteBrowseDialog.ui
   StatusBar.ui
   DialogAbout.ui
   Settings/RemoteFileDialog.ui
   Settings/AskNewPasswordDialog.ui
   Peers/PeersDock.ui
   Chat/RoomsDock.ui
   Search/SearchDock.ui
   Browse/BrowseWidget.ui
   Chat/ChatWidget.ui
   Downloads/DownloadsWidget.ui
   Search/SearchWidget.ui
   Settings/SettingsWidget.ui
   Uploads/UploadsWidget.ui
   Emoticons/SingleEmoticonWidget.ui
   AutoComplete/AutoComplete.ui

   # RESOURCES
   resources.qrc
   RemoteBrowseDialog/RemoteBrowseQuickAccessModel.h RemoteBrowseDialog/RemoteBrowseQuickAccessModel.cpp
)

set_target_properties(DLanGUI PROPERTIES
   OUTPUT_NAME "D-LAN.GUI"
   AUTOUIC ON
   # AUTOUIC looks for the .ui next to the source that includes 'ui_xxx.h'.
   # RemoteBrowseDialog.ui sits here while RemoteBrowseDialog.cpp is in the
   # RemoteBrowseDialog/ subdirectory, so the GUI root has to be searched too.
   AUTOUIC_SEARCH_PATHS "${CMAKE_CURRENT_SOURCE_DIR}"
   AUTORCC ON
)

target_include_directories(DLanGUI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(DLanGUI PRIVATE
   RemoteCoreController
   LogManager
   Common
   dlan_protos
   Qt6::Widgets
   Qt6::Xml
   Qt6::SvgWidgets
)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
   find_package(Qt6 REQUIRED COMPONENTS DBus)
   # Qt's Wayland activation-token interface is declared in private headers,
   # but implemented in QtGui itself. Only the matching headers are needed;
   # the full GuiPrivate target also requires unrelated XKB development files.
   find_path(DLAN_QT_GUI_PRIVATE_HEADERS qpa/qplatformwindow_p.h
      HINTS ${Qt6Gui_INCLUDE_DIRS} PATH_SUFFIXES "${Qt6Gui_VERSION}/QtGui" REQUIRED)
   find_path(DLAN_QT_CORE_PRIVATE_HEADERS private/qglobal_p.h
      HINTS ${Qt6Core_INCLUDE_DIRS} PATH_SUFFIXES "${Qt6Core_VERSION}/QtCore" REQUIRED)
   set(DLAN_QT_NATIVE_INCLUDE_DIRS
      "${DLAN_QT_GUI_PRIVATE_HEADERS}" "${DLAN_QT_GUI_PRIVATE_HEADERS}/.."
      "${DLAN_QT_CORE_PRIVATE_HEADERS}" "${DLAN_QT_CORE_PRIVATE_HEADERS}/..")
   target_include_directories(DLanGUI SYSTEM PRIVATE ${DLAN_QT_NATIVE_INCLUDE_DIRS})
   target_link_libraries(DLanGUI PRIVATE Qt6::DBus)

   if(DLAN_BUILD_TESTS)
      add_executable(TestsIconProvider TestsIconProvider.cpp IconProvider.cpp resources.qrc)
      set_target_properties(TestsIconProvider PROPERTIES AUTORCC ON)
      target_include_directories(TestsIconProvider PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
      target_link_libraries(TestsIconProvider PRIVATE Common Qt6::Widgets Qt6::Svg Qt6::Test)
      add_test(NAME TestsIconProvider COMMAND TestsIconProvider)
      set_tests_properties(TestsIconProvider PROPERTIES
         ENVIRONMENT "QT_QPA_PLATFORM=offscreen;QT_QPA_PLATFORMTHEME=;DBUS_SESSION_BUS_ADDRESS=unix:path=/nonexistent/dlan-icon-tests"
         TIMEOUT 20)

      add_executable(TestsFileLocations
         TestsFileLocations.cpp UtilsLocations.cpp Browse/BrowseModel.cpp
         Settings/SharedEntryListModel.cpp IconProvider.cpp Log.cpp
      )
      target_include_directories(TestsFileLocations PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
      target_include_directories(TestsFileLocations SYSTEM PRIVATE ${DLAN_QT_NATIVE_INCLUDE_DIRS})
      target_link_libraries(TestsFileLocations PRIVATE RemoteCoreController LogManager Common Qt6::Widgets Qt6::DBus Qt6::Test)
      find_program(DLAN_DBUS_RUN_SESSION dbus-run-session)
      if(DLAN_DBUS_RUN_SESSION)
         add_test(NAME TestsFileLocations COMMAND "${DLAN_DBUS_RUN_SESSION}" -- $<TARGET_FILE:TestsFileLocations>)
         set_tests_properties(TestsFileLocations PROPERTIES TIMEOUT 20)
      endif()
   endif()
endif()

if(DLAN_BUILD_TESTS)
   add_executable(TestsDownloadsTreeModel
      Downloads/TestsDownloadsTreeModel.cpp
      Downloads/DownloadsTreeModel.cpp Downloads/DownloadsModel.cpp
      Peers/PeerListModel.cpp Settings/SharedEntryListModel.cpp
      IconProvider.cpp Log.cpp resources.qrc
   )
   set_target_properties(TestsDownloadsTreeModel PROPERTIES AUTORCC ON)
   target_include_directories(TestsDownloadsTreeModel PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
   target_link_libraries(TestsDownloadsTreeModel PRIVATE RemoteCoreController LogManager Common Qt6::Widgets Qt6::Svg Qt6::Test)
   add_test(NAME TestsDownloadsTreeModel COMMAND TestsDownloadsTreeModel)
   set_tests_properties(TestsDownloadsTreeModel PROPERTIES ENVIRONMENT "QT_QPA_PLATFORM=offscreen" TIMEOUT 60)
endif()

if(WIN32)
   target_sources(DLanGUI PRIVATE
      Taskbar/TaskbarImplWin.cpp
      Taskbar/TaskbarImplWin.h
      "${CMAKE_SOURCE_DIR}/Common/version.rc"
   )
   target_link_libraries(DLanGUI PRIVATE ole32)
endif()
