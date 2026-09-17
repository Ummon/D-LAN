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
  * The device must outlive the stream. Pending data is written by Flush() or destruction;
  * callers must check Flush() to detect errors in the final write.
  */

ZeroCopyOutputStreamQIODevice::ZeroCopyOutputStreamQIODevice(QIODevice* device) :
   writer(device), adapter(&this->writer, Constants::PROTOBUF_STREAMING_BUFFER_SIZE)
{
   Q_ASSERT(device);
}

bool ZeroCopyOutputStreamQIODevice::DeviceWriter::Write(const void* buffer, int size)
{
   if (this->failed)
      return false;

   const char* data = static_cast<const char*>(buffer);
   while (size > 0)
   {
      const qint64 written = this->device->write(data, size);
      if (written <= 0)
      {
         this->failed = true;
         return false;
      }

      data += written;
      size -= static_cast<int>(written);
   }

   return true;
}

bool ZeroCopyOutputStreamQIODevice::Next(void** data, int* size)
{
   // Some protobuf versions can return a new buffer after a failed flush.
   return !this->writer.failed && this->adapter.Next(data, size);
}

void ZeroCopyOutputStreamQIODevice::BackUp(int count)
{
   this->adapter.BackUp(count);
}

google::protobuf::int64 ZeroCopyOutputStreamQIODevice::ByteCount() const
{
   return this->adapter.ByteCount();
}

bool ZeroCopyOutputStreamQIODevice::Flush()
{
   return this->adapter.Flush();
}

/**
  * @class Common::ZeroCopyInputStreamQIODevice
  *
  * A bridge to read data from a QIODevice by a google::protobuf::message.
  * Consumes completed peek buffers as it advances. Destruction consumes only the
  * used part of the final peek, leaving backed-up bytes available to the device.
  */

ZeroCopyInputStreamQIODevice::ZeroCopyInputStreamQIODevice(QIODevice* device) :
   device(device), nbLastRead(0), pos(buffer), bytesRead(0)
{
}

ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice()
{
   this->device->skip(this->pos - this->buffer);
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

   const qint64 skipped = this->device->skip(count);
   if (skipped < 0)
      return false;
   this->bytesRead += skipped;
   return skipped == count;
}

/**
  * Takes out of the device the data of the current peek, it must all have been given to the consumer.
  */
void ZeroCopyInputStreamQIODevice::consumeCurrentPeek()
{
   if (this->nbLastRead != 0)
   {
      this->device->skip(this->nbLastRead);
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
