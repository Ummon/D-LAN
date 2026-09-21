# ==============================================================================
# Common static library (was Common/Common.pro).
# The ../Protos/*.pb.cc sources are now provided by the 'dlan_protos' target.
# ==============================================================================

add_library(Common STATIC
   Global.cpp
   ZeroCopyStreamQIODevice.cpp
   Settings.cpp
   TransferRateCalculator.cpp
   ProtoHelper.cpp
   Timeoutable.cpp
   PersistentData.cpp
   Network/MessageSocket.cpp
   Network/RemoteControlTls.cpp
   Network/MessageHeader.cpp
   ThreadPool.cpp
   Languages.cpp
   Constants.cpp
   FileLocker.cpp
   ConsoleReader.cpp
   StringUtils.cpp
   Network/Message.cpp
   KnownExtensions.cpp
   Hash_noShare.cpp
   Hash_share.cpp
   Path.cpp

   Hash.h
   Constants.h
   Global.h
   Uncopyable.h
   ZeroCopyStreamQIODevice.h
   Settings.h
   TransferRateCalculator.h
   ProtoHelper.h
   Timeoutable.h
   Version.h
   PersistentData.h
   Network/MessageSocket.h
   Network/RemoteControlTls.h
   Network/MessageHeader.h
   ThreadPool.h
   IRunnable.h
   Languages.h
   FileLocker.h
   ConsoleReader.h
   StringUtils.h
   BloomFilter.h
   Network/Message.h
   Network/InterfacePolicy.h
   KnownExtensions.h
   Containers/Tree.h
   Containers/SortedList.h
   Containers/SortedArray.h
   Containers/MapArray.h
   SelfWeakPointer.h
   Hash_noShare.h
   Hash_share.h
   Path.h
   SharedEntry.h
)

target_compile_definitions(Common PRIVATE COMMON_LIBRARY)

# The project root, so that '#include <Common/...>' and '#include <Protos/...>'
# resolve for every consumer (was INCLUDEPATH += . ..).
target_include_directories(Common PUBLIC
   "${CMAKE_SOURCE_DIR}"
   "${CMAKE_CURRENT_SOURCE_DIR}"
)

target_link_libraries(Common PUBLIC
   Qt6::Core
   Qt6::Network
   dlan_protos
   blake3
)

# Qt handles TLS transport; libcrypto generates and validates the persistent
# self-signed Core identity without invoking an external openssl executable.
find_package(OpenSSL 3 REQUIRED COMPONENTS Crypto SSL)
target_link_libraries(Common PRIVATE OpenSSL::Crypto)

if(APPLE)
   target_link_libraries(Common PRIVATE "-framework SystemConfiguration" "-framework CoreFoundation")
endif()

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsCommon)
endif()
