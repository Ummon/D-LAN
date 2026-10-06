# ==============================================================================
# D-LAN.DummyCore executable: a Core without any logic, it always gives the same
# state to the connected GUI. The state (peers, files, search result, downloads
# and uploads) is read from a JSON file. It's used to make screenshots of the
# GUI. See main.cpp for the usage and State.h for the JSON format.
# ==============================================================================

set(DLAN_DUMMY_CORE_SOURCES
   State.cpp
   Server.cpp
   Connection.cpp

   State.h
   Server.h
   Connection.h
)

add_executable(DLanDummyCore main.cpp ${DLAN_DUMMY_CORE_SOURCES})

set_target_properties(DLanDummyCore PROPERTIES OUTPUT_NAME "D-LAN.DummyCore")

target_include_directories(DLanDummyCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

target_link_libraries(DLanDummyCore PRIVATE
   Common
   dlan_protos
   Qt6::Core
   Qt6::Network
)

# The examples of state, next to the executable. 'DummyCore.example.<language>.json' is the same state as
# 'DummyCore.example.json' with the peer nicks and the folder names of the given language.
file(GLOB DLAN_DUMMY_CORE_EXAMPLES CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/DummyCore.example*.json")
foreach(_example IN LISTS DLAN_DUMMY_CORE_EXAMPLES)
   get_filename_component(_name "${_example}" NAME)
   configure_file("${_example}" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${_name}" COPYONLY)
endforeach()

if(DLAN_BUILD_TESTS)
   add_executable(TestsDummyCore TestsDummyCore/Tests.cpp ${DLAN_DUMMY_CORE_SOURCES})
   target_include_directories(TestsDummyCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
   target_compile_definitions(TestsDummyCore PRIVATE
      DUMMY_CORE_EXAMPLES_DIRECTORY="${CMAKE_CURRENT_SOURCE_DIR}")
   target_link_libraries(TestsDummyCore PRIVATE RemoteCoreController Qt6::Test)
   add_test(NAME TestsDummyCore COMMAND TestsDummyCore)
   set_tests_properties(TestsDummyCore PROPERTIES TIMEOUT 30)
endif()
