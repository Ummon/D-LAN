# ==============================================================================
# Common unit tests (was Common/TestsCommon/TestsCommon.pro).
# ==============================================================================

add_executable(TestsCommon
   main.cpp
   Tests.cpp
   TreeTests.cpp
   BenchmarkTests.cpp

   Tests.h
   TreeTests.h
   BenchmarkTests.h
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
   target_compile_options(TestsCommon PRIVATE -Wno-pessimizing-move -Wno-unused-result)
endif()

target_include_directories(TestsCommon PRIVATE
   "${CMAKE_CURRENT_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}/.."
)

target_link_libraries(TestsCommon PRIVATE
   Common
   LogManager
   Qt6::Test
)

add_test(NAME TestsCommon COMMAND TestsCommon)

add_executable(TestsSortedList SortedListTests.cpp)
target_include_directories(TestsSortedList PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")
target_link_libraries(TestsSortedList PRIVATE Qt6::Test)
add_test(NAME TestsSortedList COMMAND TestsSortedList)

add_executable(TestsSortedArrayAllocation SortedArrayAllocationTests.cpp)
target_include_directories(TestsSortedArrayAllocation PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")
target_link_libraries(TestsSortedArrayAllocation PRIVATE Qt6::Test)
add_test(NAME TestsSortedArrayAllocation COMMAND TestsSortedArrayAllocation)

add_executable(TestsSortedArrayInvariants SortedArrayInvariantTests.cpp)
target_include_directories(TestsSortedArrayInvariants PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")
target_link_libraries(TestsSortedArrayInvariants PRIVATE Qt6::Test)
add_test(NAME TestsSortedArrayInvariants COMMAND TestsSortedArrayInvariants)

add_executable(TestsTransferRateCalculator TransferRateCalculatorTests.cpp)
target_link_libraries(TestsTransferRateCalculator PRIVATE Common Qt6::Test)
add_test(NAME TestsTransferRateCalculator COMMAND TestsTransferRateCalculator)

add_executable(TestsSettings SettingsTests.cpp)
target_link_libraries(TestsSettings PRIVATE Common Qt6::Test)
add_test(NAME TestsSettings COMMAND TestsSettings)
set_tests_properties(TestsSettings PROPERTIES TIMEOUT 30)

add_executable(TestsPersistentData PersistentDataTests.cpp)
target_link_libraries(TestsPersistentData PRIVATE Common Qt6::Test)
add_test(NAME TestsPersistentData COMMAND TestsPersistentData)

add_executable(TestsKnownExtensions KnownExtensionsTests.cpp)
target_link_libraries(TestsKnownExtensions PRIVATE Common Qt6::Test)
add_test(NAME TestsKnownExtensions COMMAND TestsKnownExtensions)

add_executable(TestsStringUtils StringUtilsTests.cpp)
target_link_libraries(TestsStringUtils PRIVATE Common Qt6::Test)
add_test(NAME TestsStringUtils COMMAND TestsStringUtils)

add_executable(TestsZeroCopyOutputStreamQIODevice ZeroCopyOutputStreamQIODeviceTests.cpp)
target_link_libraries(TestsZeroCopyOutputStreamQIODevice PRIVATE Common Qt6::Test)
add_test(NAME TestsZeroCopyOutputStreamQIODevice COMMAND TestsZeroCopyOutputStreamQIODevice)

add_executable(TestsZeroCopyInputStreamQIODevice ZeroCopyInputStreamQIODeviceTests.cpp)
target_link_libraries(TestsZeroCopyInputStreamQIODevice PRIVATE Common Qt6::Test)
add_test(NAME TestsZeroCopyInputStreamQIODevice COMMAND TestsZeroCopyInputStreamQIODevice)

add_executable(TestsMessageSocket MessageSocketTests.cpp)
target_link_libraries(TestsMessageSocket PRIVATE Common Qt6::Test)
add_test(NAME TestsMessageSocket COMMAND TestsMessageSocket)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
   add_executable(TestsGlobalLinux GlobalLinuxTests.cpp)
   target_link_libraries(TestsGlobalLinux PRIVATE Common Qt6::Test)
   target_link_options(TestsGlobalLinux PRIVATE "LINKER:--wrap=statvfs")
   add_test(NAME TestsGlobalLinux COMMAND TestsGlobalLinux)
endif()

if(UNIX)
   add_executable(TestsConsoleReader ConsoleReaderTests.cpp)
   target_link_libraries(TestsConsoleReader PRIVATE Common Qt6::Test)
   add_test(NAME TestsConsoleReader COMMAND TestsConsoleReader)
   set_tests_properties(TestsConsoleReader PROPERTIES TIMEOUT 10)
elseif(WIN32)
   add_executable(TestsConsoleReader ConsoleReaderWindowsTests.cpp)
   target_link_libraries(TestsConsoleReader PRIVATE Common Qt6::Test)
   add_test(NAME TestsConsoleReader COMMAND TestsConsoleReader)
   set_tests_properties(TestsConsoleReader PROPERTIES TIMEOUT 10)
endif()

if(APPLE)
   add_executable(TestsGlobalDarwin GlobalDarwinTests.cpp)
   target_link_libraries(TestsGlobalDarwin PRIVATE Common Qt6::Test)
   add_test(NAME TestsGlobalDarwin COMMAND TestsGlobalDarwin)

   add_executable(TestsGlobalDarwinIdentity GlobalDarwinIdentityTests.cpp)
   target_link_libraries(TestsGlobalDarwinIdentity PRIVATE Common Qt6::Test)
   add_test(NAME TestsGlobalDarwinIdentity COMMAND TestsGlobalDarwinIdentity)

   add_executable(TestsGlobalDarwinPaths GlobalDarwinPathsTests.cpp)
   target_link_libraries(TestsGlobalDarwinPaths PRIVATE Common LogManager HashCache Qt6::Test)
   add_test(NAME TestsGlobalDarwinPaths COMMAND TestsGlobalDarwinPaths)
   set_tests_properties(TestsGlobalDarwinPaths PROPERTIES TIMEOUT 30)
endif()
