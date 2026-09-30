# ==============================================================================
# StressTests executable: launches some D-LAN Cores, each one supervised by a
# CoreSupervisor in its own thread, and executes random actions on them through
# the remote control (RemoteCoreController). See main.cpp for the usage.
# ==============================================================================

add_executable(StressTests
   main.cpp
   Log.cpp
   Config.cpp
   DiskBudget.cpp
   CoreSupervisor.cpp
   StressRun.cpp
   SearchCoordinator.cpp
   Paths.cpp

   Log.h
   Config.h
   DiskBudget.h
   CoreSupervisor.h
   StressRun.h
   SearchCoordinator.h
   Paths.h
)

target_include_directories(StressTests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(StressTests PRIVATE
   RemoteCoreController
   LogManager
   Common
   dlan_protos
   Qt6::Core
   Qt6::Network
)

# The Cores are launched from the same directory.
add_dependencies(StressTests DLanCore)

# An example of configuration with all the default values, next to the executable.
configure_file(StressTests.example.json "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/StressTests.example.json" COPYONLY)
