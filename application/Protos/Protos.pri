# A loop doesn't work :/
#PROTOS = common core_protocol
#for(proto, PROTOS) {
#   proto_$${proto}.target = $$PWD/$${proto}.pb.cc
#   proto_$${proto}.depends = $$PWD/$${proto}.proto
#   proto_$${proto}.commands = cd $$PWD; protoc --cpp_out . $${proto}.proto
#   QMAKE_EXTRA_TARGETS += proto_$${proto}
#   PRE_TARGETDEPS += $$PWD/$${proto}.pb.cc
#}

proto_queue.target = $$PWD/queue.pb.cc
proto_queue.depends = $$PWD/queue.proto
proto_queue.commands = cd $$PWD && protoc --cpp_out . queue.proto

proto_gui_settings.target = $$PWD/gui_settings.pb.cc
proto_gui_settings.depends = $$PWD/gui_settings.proto
proto_gui_settings.commands = cd $$PWD && protoc --cpp_out . gui_settings.proto

proto_gui_protocol.target = $$PWD/gui_protocol.pb.cc
proto_gui_protocol.depends = $$PWD/gui_protocol.proto
proto_gui_protocol.commands = cd $$PWD && protoc --cpp_out . gui_protocol.proto

proto_core_settings.target = $$PWD/core_settings.pb.cc
proto_core_settings.depends = $$PWD/core_settings.proto
proto_core_settings.commands = cd $$PWD && protoc --cpp_out . core_settings.proto

proto_core_protocol.target = $$PWD/core_protocol.pb.cc
proto_core_protocol.depends = $$PWD/core_protocol.proto
proto_core_protocol.commands = cd $$PWD && protoc --cpp_out . core_protocol.proto

proto_common.target = $$PWD/common.pb.cc
proto_common.depends = $$PWD/common.proto
proto_common.commands = cd $$PWD && protoc --cpp_out . common.proto

QMAKE_EXTRA_TARGETS += proto_queue proto_gui_settings proto_gui_protocol proto_files_cache proto_core_settings proto_core_protocol proto_common
PRE_TARGETDEPS += $$PWD/queue.pb.cc $$PWD/gui_settings.pb.cc $$PWD/gui_protocol.pb.cc $$PWD/files_cache.pb.cc $$PWD/core_settings.pb.cc $$PWD/core_protocol.pb.cc $$PWD/common.pb.cc

OTHER_FILES += \
   $$PWD/queue.proto \
   $$PWD/gui_settings.proto \
   $$PWD/gui_protocol.proto \
   $$PWD/files_cache.proto \
   $$PWD/core_settings.proto \
   $$PWD/core_protocol.proto \
   $$PWD/common.proto
