win32 {
   PROTOBUF = c:/protobuf
   LIBS += -L$$PROTOBUF/src/.libs -lprotobuf
   INCLUDEPATH += $$PROTOBUF/src
}

unix {
   LIBS += -lprotobuf
}
