#ifndef COMMON_ZERO_COPY_STREAM_QIODEVICE_H
#define COMMON_ZERO_COPY_STREAM_QIODEVICE_H

#include <QIODevice>

#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/stubs/common.h>

#include "Constants.h"
#include "Uncopyable.h"

namespace Common
{
   class ZeroCopyOutputStreamQIODevice : public google::protobuf::io::ZeroCopyOutputStream, Uncopyable
   {
   public:
      ZeroCopyOutputStreamQIODevice(QIODevice* device);
      ~ZeroCopyOutputStreamQIODevice();
      bool Next(void** data, int* size);
      void BackUp(int count);
      google::protobuf::int64 ByteCount() const;

   private:
      QIODevice* device;
      char buffer[PROTOBUF_STREAMING_BUFFER_SIZE];
      char* pos;
      google::protobuf::int64 bytesWritten;
   };

   class ZeroCopyInputStreamQIODevice : public google::protobuf::io::ZeroCopyInputStream, Uncopyable
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
      char* pos; ///< Point on the remaining data, remaing data size is "buffer + nbLastRead - pos".

      google::protobuf::int64 bytesRead;
   };
}

#endif
