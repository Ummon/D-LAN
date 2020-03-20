FLATBUFFERS_PARAMS = "--cpp --scoped-enums --filename-suffix _fbs"
FLATC = "..\Tools\flatc.exe"

fbs_queue.target = $$PWD/queue_fbs.h
fbs_queue.depends = $$PWD/queue.fbs
fbs_queue.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS queue.fbs

fbs_gui_settings.target = $$PWD/gui_settings_fbs.h
fbs_gui_settings.depends = $$PWD/gui_settings.fbs
fbs_gui_settings.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS gui_settings.fbs

fbs_gui_protocol.target = $$PWD/gui_protocol_fbs.h
fbs_gui_protocol.depends = $$PWD/gui_protocol.fbs
fbs_gui_protocol.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS gui_protocol.fbs

fbs_files_cache.target = $$PWD/files_cache_fbs.h
fbs_files_cache.depends = $$PWD/files_cache.fbs
fbs_files_cache.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS files_cache.fbs

fbs_core_settings.target = $$PWD/core_settings_fbs.h
fbs_core_settings.depends = $$PWD/core_settings.fbs
fbs_core_settings.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS core_settings.fbs

fbs_core_protocol.target = $$PWD/core_protocol_fbs.h
fbs_core_protocol.depends = $$PWD/core_protocol.fbs
fbs_core_protocol.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS core_protocol.fbs

fbs_common.target = $$PWD/common_fbs.h
fbs_common.depends = $$PWD/common.fbs
fbs_common.commands = cd $$PWD && $$FLATC $$FLATBUFFERS_PARAMS common.fbs

INCLUDEPATH += $$PWD/../Libs

HEADERS += $$PWD/../Libs/flatbuffers.h

QMAKE_EXTRA_TARGETS += fbs_queue fbs_gui_settings fbs_gui_protocol fbs_files_cache fbs_core_settings fbs_core_protocol fbs_common
PRE_TARGETDEPS += $$PWD/queue_fbs.h $$PWD/gui_settings_fbs.h $$PWD/gui_protocol_fbs.h $$PWD/files_cache_fbs.h $$PWD/core_settings_fbs.h $$PWD/core_protocol_fbs.h $$PWD/common_fbs.h

OTHER_FILES += \
   $$PWD/queue.fbs \
   $$PWD/gui_settings.fbs \
   $$PWD/gui_protocol.fbs \
   $$PWD/files_cache.fbs \
   $$PWD/core_settings.fbs \
   $$PWD/core_protocol.fbs \
   $$PWD/common.fbs
