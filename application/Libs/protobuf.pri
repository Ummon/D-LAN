win32 {
   PROTOBUF = c:/protobuf-3.11.4
   LIBS += -L$$PROTOBUF/src/.libs -lprotobuf
   INCLUDEPATH += $$PROTOBUF/src
}

unix {
   LIBS += -lprotobuf
}
