win32 {
   PROTOBUF = c:/protobuf
   PROTOBUF_BUILD = $$PROTOBUF/build

   LIBS += $$system(pkg-config --define-variable=prefix=C:/protobuf --static --libs protobuf)
   QMAKE_LFLAGS += $$system(pkg-config --static --libs protobuf)
   CONFIG += link_pkgconfig
   PKGCONFIG += protobuf
}

unix {
   LIBS += -lprotobuf
}
