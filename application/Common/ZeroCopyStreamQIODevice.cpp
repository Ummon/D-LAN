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

      this->bytesWritten += nBytes;

      // The whole buffer is expected to be written. If not the stream can't be used any further,
      // 'false' tells the caller the serialization has failed.
      if (nBytes != this->pos - this->buffer)
      {
         this->pos = this->buffer;
         return false;
      }

      this->pos = this->buffer;
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

   // Everything peeked has been given to the consumer, it can now be taken out of the device.
   this->consumeCurrentPeek();

   this->nbLastRead = this->device->peek(this->buffer, Constants::PROTOBUF_STREAMING_BUFFER_SIZE);
   if (this->nbLastRead <= 0)
   {
      this->nbLastRead = 0;
      this->pos = this->buffer;
      return false;
   }

   *data = this->buffer;
   *size = this->nbLastRead;

   this->pos = this->buffer + this->nbLastRead;

   return true;
}

void ZeroCopyInputStreamQIODevice::BackUp(int count)
{
   Q_ASSERT(count >= 0);

   this->pos -= count;

   if (this->pos < this->buffer)
      this->pos = this->buffer;
}

bool ZeroCopyInputStreamQIODevice::Skip(int count)
{
   if (count < 0)
      return false;

   // First the data still in the buffer. See 'BackUp(..)'.
   const int nbBytesInBuffer = this->nbLastRead - static_cast<int>(this->pos - this->buffer);
   if (nbBytesInBuffer > 0)
   {
      const int nbBytesSkipped = qMin(count, nbBytesInBuffer);
      this->pos += nbBytesSkipped; // Counted by 'ByteCount()' via 'pos'.
      count -= nbBytesSkipped;
   }

   if (count == 0)
      return true;

   // The peeked data is still in the device, it must be taken out before skipping the bytes which follow it.
   this->consumeCurrentPeek();

   const QByteArray data = this->device->read(count);
   this->bytesRead += data.size();
   return data.size() == count;
}

/**
  * Takes out of the device the data of the current peek, it must all have been given to the consumer.
  */
void ZeroCopyInputStreamQIODevice::consumeCurrentPeek()
{
   if (this->nbLastRead != 0)
   {
      this->device->read(this->nbLastRead);
      this->bytesRead += this->nbLastRead;
      this->nbLastRead = 0;
   }

   this->pos = this->buffer;
}

/**
  * @return The number of bytes given to the consumer: the ones already taken out of the device plus the
  *         consumed part of the current peek. 'BackUp(..)' moves 'pos' back, thus it's taken into account.
  */
google::protobuf::int64 ZeroCopyInputStreamQIODevice::ByteCount() const
{
   return this->bytesRead + (this->pos - this->buffer);
}
