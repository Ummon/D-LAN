win32 {
   PROTOBUF = d:/protobuf
   LIBS += -L$$PROTOBUF/src/.libs -lprotobuf
   INCLUDEPATH += $$PROTOBUF/src
}

unix {
   LIBS += -lprotobuf
}
