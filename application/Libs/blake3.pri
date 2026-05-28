win32 {
   BLAKE3 = C:/BLAKE3-1.8.5/c

   LIBS += -L$$BLAKE3/lib -lblake3
   INCLUDEPATH += $$BLAKE3
}
