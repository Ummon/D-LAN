/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#pragma once

#include <QIODevice>

#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/io/zero_copy_stream_impl_lite.h>
#include <google/protobuf/stubs/common.h>

#include <Common/Uncopyable.h>
#include <Common/Constants.h>

namespace Common
{
   class ZeroCopyOutputStreamQIODevice : public google::protobuf::io::ZeroCopyOutputStream
   {
   public:
      explicit ZeroCopyOutputStreamQIODevice(QIODevice* device);
      bool Next(void** data, int* size) override;
      void BackUp(int count) override;
      google::protobuf::int64 ByteCount() const override;

      // Writes pending bytes to the device, without waiting for the device itself to flush.
      // Call after serialization to detect errors in the final buffered write.
      bool Flush();

   private:
      struct DeviceWriter : google::protobuf::io::CopyingOutputStream
      {
         explicit DeviceWriter(QIODevice* device) : device(device) {}
         bool Write(const void* buffer, int size) override;

         QIODevice* device;
         bool failed = false;
      };

      DeviceWriter writer;
      // Destroyed first, so its final flush can still use writer.
      google::protobuf::io::CopyingOutputStreamAdaptor adapter;
   };

   class ZeroCopyInputStreamQIODevice : public google::protobuf::io::ZeroCopyInputStream, Uncopyable
   {
   public:
      ZeroCopyInputStreamQIODevice(QIODevice* device);
      ~ZeroCopyInputStreamQIODevice();

      bool Next(const void** data, int* size) override;
      void BackUp(int count) override;
      bool Skip(int count) override;
      google::protobuf::int64 ByteCount() const override;

   private:
      void consumeCurrentPeek();

      QIODevice* device;

      int nbLastRead;
      char buffer[Constants::PROTOBUF_STREAMING_BUFFER_SIZE];
      char* pos; ///< Point on the remaining data, remaining data size is "buffer + nbLastRead - pos".

      google::protobuf::int64 bytesRead;
   };
}
