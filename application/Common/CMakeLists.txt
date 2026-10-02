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
   Network/RemoteControlAuthentication.cpp
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
   SaltedPassword.cpp

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
   Network/RemoteControlAuthentication.h
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
   SaltedPassword.h
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
# It also derives the remote control keys with Argon2id, available since OpenSSL 3.2.
find_package(OpenSSL 3.2 REQUIRED COMPONENTS Crypto SSL)
if(EXISTS "${OPENSSL_INCLUDE_DIR}/stddef.h")
   # A toolchain's C headers would shadow libc++'s ones ('<cstddef> tried including <stddef.h>').
   message(FATAL_ERROR
      "OPENSSL_INCLUDE_DIR (${OPENSSL_INCLUDE_DIR}) contains C library headers from another toolchain. "
      "Use an OpenSSL installation whose include directory holds only OpenSSL, such as MSYS2 "
      "'mingw-w64-clang-x86_64-openssl' with OPENSSL_ROOT_DIR=C:/msys64/clang64, then clear the "
      "cached OPENSSL_* entries.")
endif()
target_link_libraries(Common PRIVATE OpenSSL::Crypto)

if(WIN32)
   # Copy the DLLs next to the executables so running from the build directory does not
   # pick up an incompatible libcrypto/libssl with the same name from PATH.
   if(DLAN_OPENSSL_RUNTIME_DIR)
      set(_dlan_openssl_bin "${DLAN_OPENSSL_RUNTIME_DIR}")
   else()
      get_filename_component(_dlan_openssl_bin "${OPENSSL_CRYPTO_LIBRARY}/../../bin" ABSOLUTE)
   endif()
   file(GLOB _dlan_openssl_dlls "${_dlan_openssl_bin}/libcrypto-3*.dll" "${_dlan_openssl_bin}/libssl-3*.dll")
   file(COPY ${_dlan_openssl_dlls} DESTINATION "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
endif()

if(APPLE)
   target_link_libraries(Common PRIVATE "-framework SystemConfiguration" "-framework CoreFoundation")
endif()

if(DLAN_BUILD_TESTS)
   add_subdirectory(TestsCommon)
endif()
