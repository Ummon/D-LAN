win32 {
   PROTOBUF = e:/protobuf
   LIBS += -L$$PROTOBUF/src/.libs -lprotobuf
   INCLUDEPATH += $$PROTOBUF/src
}

unix {
   LIBS += -lprotobuf
}
