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

#include <Common/ZeroCopyStreamQIODevice.h>
using namespace Common;

#include <Common/Constants.h>

/**
  * @class Common::ZeroCopyOutputStreamQIODevice
  *
  * A bridge to write data to a QIODevice from a google::protobuf::message.
  * Warning : The data will be effectively written when the object is destroyed.
  */

ZeroCopyOutputStreamQIODevice::ZeroCopyOutputStreamQIODevice(QIODevice* device) :
   device(device), buffer(new char[Constants::PROTOBUF_STREAMING_BUFFER_SIZE]), bytesWritten(0)
{
   this->pos = this->buffer;
}

ZeroCopyOutputStreamQIODevice::~ZeroCopyOutputStreamQIODevice()
{
   if (this->pos != this->buffer)
      this->device->write(this->buffer, this->pos - this->buffer);
   delete[] this->buffer;
}

bool ZeroCopyOutputStreamQIODevice::Next(void** data, int* size)
{
   if (this->pos != this->buffer)
   {
      int nBytes = this->device->write(this->buffer, this->pos - this->buffer);
      if (nBytes == -1)
         return false;

      // We assume that all the buffer is written.
      if (nBytes != this->pos - this->buffer)
         throw 1;

      this->pos -= nBytes;
      this->bytesWritten += nBytes;
   }

   *data = this->buffer;
   *size = Constants::PROTOBUF_STREAMING_BUFFER_SIZE;

   this->pos = this->buffer + Constants::PROTOBUF_STREAMING_BUFFER_SIZE;

   return true;
}

void ZeroCopyOutputStreamQIODevice::BackUp(int count)
{
   this->pos -= count;

   if (this->pos < this->buffer)
      this->pos = this->buffer;
}

google::protobuf::int64 ZeroCopyOutputStreamQIODevice::ByteCount() const
{
   return this->bytesWritten;
}

/**
  * @class Common::ZeroCopyInputStreamQIODevice
  *
  * A bridge to read data from a QIODevice by a google::protobuf::message.
  * Warning : The data will be effectively read when the object is destroyed.
  */

ZeroCopyInputStreamQIODevice::ZeroCopyInputStreamQIODevice(QIODevice* device) :
   device(device), nbLastRead(0), buffer(new char[Constants::PROTOBUF_STREAMING_BUFFER_SIZE]), pos(buffer), bytesRead(0)
{
}

ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice()
{
   this->device->read(this->pos - this->buffer);
   delete[] this->buffer;
}

bool ZeroCopyInputStreamQIODevice::Next(const void** data, int* size)
{
   if (this->pos != this->buffer + this->nbLastRead) // There is still some data into the buffer. See 'BackUp(..)'.
   {
      *data = this->pos;
      *size = this->nbLastRead - (this->pos - this->buffer);
      this->pos = this->buffer + this->nbLastRead;
      return true;
   }

   if (this->nbLastRead != 0)
      this->device->read(this->nbLastRead);

   this->nbLastRead = this->device->peek(this->buffer, Constants::PROTOBUF_STREAMING_BUFFER_SIZE);
   if (this->nbLastRead <= 0)
   {
      this->pos = this->buffer;
      return false;
   }
   this->bytesRead += this->nbLastRead;

   *data = this->buffer;
   *size = this->nbLastRead;

   this->pos = this->buffer + this->nbLastRead;

   return true;
}

void ZeroCopyInputStreamQIODevice::BackUp(int count)
{
   this->pos -= count;
}

bool ZeroCopyInputStreamQIODevice::Skip(int count)
{
   if (this->pos != this->buffer + this->nbLastRead) // There is still some data into the buffer. See 'BackUp(..)'.
   {
      if (this->pos + count > this->buffer + this->nbLastRead)
      {
         count -= (this->buffer + this->nbLastRead) - this->pos;
         this->pos = this->buffer + this->nbLastRead;
      }
      else
      {
         this->pos += count;
         count = 0;
      }
   }

   if (this->device->bytesAvailable() == 0)
      return false;

   if (count == 0)
      return true;

   QByteArray data = this->device->read(count);
   if (data.isNull())
      return false;
   this->bytesRead += data.size();
   return this->device->bytesAvailable() > 0;
}

google::protobuf::int64 ZeroCopyInputStreamQIODevice::ByteCount() const
{
   return this->bytesRead;
}
