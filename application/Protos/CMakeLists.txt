# ==============================================================================
# Protocol buffer messages (was Protos/Protos.pri).
#
# Differences from the qmake build:
#  - Generated files go to the build tree (${CMAKE_BINARY_DIR}/generated/Protos)
#    instead of polluting the source tree. '#include <Protos/xxx.pb.h>' keeps
#    working because '<build>/generated' is a public include directory.
#  - The .pb.cc files are compiled ONCE into the 'dlan_protos' static library
#    instead of being recompiled by every consumer.
#  - files_cache.proto is NOT listed here: the file was deleted with the
#    hashcache refactoring (00697243). Protos.pri still mentioned it in
#    OTHER_FILES and in QMAKE_EXTRA_TARGETS (with no rule behind it), which is
#    why qmake never complained.
# ==============================================================================

set(DLAN_PROTO_NAMES
   common
   core_protocol
   core_settings
   gui_protocol
   gui_settings
   queue
)

set(DLAN_PROTO_GEN_DIR "${CMAKE_BINARY_DIR}/generated/Protos")
file(MAKE_DIRECTORY "${DLAN_PROTO_GEN_DIR}")

set(DLAN_ALL_PROTO_FILES "")
foreach(_name IN LISTS DLAN_PROTO_NAMES)
   list(APPEND DLAN_ALL_PROTO_FILES "${CMAKE_CURRENT_SOURCE_DIR}/${_name}.proto")
endforeach()

set(DLAN_PROTO_GENERATED_SOURCES "")
foreach(_name IN LISTS DLAN_PROTO_NAMES)
   add_custom_command(
      OUTPUT
         "${DLAN_PROTO_GEN_DIR}/${_name}.pb.cc"
         "${DLAN_PROTO_GEN_DIR}/${_name}.pb.h"
      COMMAND "${DLAN_PROTOC}"
         # Quote the whole argument: '--cpp_out="${dir}"' would keep the double
         # quotes literally and protoc would read them as a generator option.
         "--cpp_out=${DLAN_PROTO_GEN_DIR}"
         -I "${CMAKE_CURRENT_SOURCE_DIR}"
         "${CMAKE_CURRENT_SOURCE_DIR}/${_name}.proto"
      # Depend on every .proto: imports between messages (e.g. common.proto)
      # must be present for protoc, and over-depending only costs a re-run.
      DEPENDS ${DLAN_ALL_PROTO_FILES}
      COMMENT "protoc: ${_name}.proto"
      VERBATIM
   )
   list(APPEND DLAN_PROTO_GENERATED_SOURCES "${DLAN_PROTO_GEN_DIR}/${_name}.pb.cc")
endforeach()

add_library(dlan_protos STATIC ${DLAN_PROTO_GENERATED_SOURCES})

set_target_properties(dlan_protos PROPERTIES AUTOMOC OFF)

target_include_directories(dlan_protos PUBLIC "${CMAKE_BINARY_DIR}/generated")
target_link_libraries(dlan_protos PUBLIC ${DLAN_PROTOBUF_TARGET})
