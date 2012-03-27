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
  
#ifndef COMMON_ZERO_COPY_STREAM_QIODEVICE_H
#define COMMON_ZERO_COPY_STREAM_QIODEVICE_H

#include <QIODevice>
#include <QVector>

#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/stubs/common.h>

#include <Common/Uncopyable.h>

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
      char* buffer;
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
      char* buffer;
      char* pos; ///< Point on the remaining data, remaing data size is "buffer + nbLastRead - pos".

      google::protobuf::int64 bytesRead;
   };
}

#endif
