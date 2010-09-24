#ifndef COMMON_ZERO_COPY_STREAM_QIODEVICE_H
#define COMMON_ZERO_COPY_STREAM_QIODEVICE_H

#include <QIODevice>

#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/stubs/common.h>

#include "Constants.h"

namespace Common
{
   class ZeroCopyOutputStreamQIODevice : public google::protobuf::io::ZeroCopyOutputStream
   {
   public:
      ZeroCopyOutputStreamQIODevice(QIODevice* device);
      bool Next(void** data, int* size);
      void BackUp(int count);
      google::protobuf::int64 ByteCount() const;

   private:
      QIODevice* device;
      bool bufferAvaible;
      char buffer[PROTOBUF_STREAMING_BUFFER_SIZE];
      google::protobuf::int64 bytesWritten;
   };

   class ZeroCopyInputStreamQIODevice : public google::protobuf::io::ZeroCopyInputStream
   {
   public:
      ZeroCopyInputStreamQIODevice(QIODevice* device);
      ~ZeroCopyInputStreamQIODevice();

      bool Next(const void** data, int* size);
      void BackUp(int count);
      bool Skip(int count);
      google::protobuf::int64 ByteCount() const;

   private:
      QIODevice* device;

      int nbLastRead;
      char buffer[PROTOBUF_STREAMING_BUFFER_SIZE];
      char* pos;

      google::protobuf::int64 bytesRead;
   };
}

#endif
