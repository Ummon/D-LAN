#include <ZeroCopyStreamQIODevice.h>
using namespace Common;

ZeroCopyOutputStreamQIODevice::ZeroCopyOutputStreamQIODevice(QIODevice* device)
   : device(device), bufferAvaible(false), bytesWritten(0)
{
}


bool ZeroCopyOutputStreamQIODevice::Next(void** data, int* size)
{
   if (this->bufferAvaible)
   {
      int nBytes = this->device->write(this->buffer, PROTOBUF_STREAMING_BUFFER_SIZE);
      if (nBytes == -1)
         return false;
      this->bytesWritten += nBytes;
   }

   this->bufferAvaible = true;
   *data = this->buffer;
   *size = PROTOBUF_STREAMING_BUFFER_SIZE;

   return true;
}

void ZeroCopyOutputStreamQIODevice::BackUp(int count)
{
   if (this->bufferAvaible)
   {
      int nBytes = this->device->write(this->buffer, PROTOBUF_STREAMING_BUFFER_SIZE - count);
      if (nBytes != -1)
         this->bytesWritten += count;
   }
}

google::protobuf::int64 ZeroCopyOutputStreamQIODevice::ByteCount() const
{
   return this->bytesWritten;
}


ZeroCopyInputStreamQIODevice::ZeroCopyInputStreamQIODevice(QIODevice* device)
   : device(device), nbLastRead(0), pos(buffer), bytesRead(0)
{
}

ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice()
{
   this->device->read(this->pos - this->buffer);
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

   this->nbLastRead = this->device->peek(this->buffer, PROTOBUF_STREAMING_BUFFER_SIZE);
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
   this->pos -= count + 1; // Don't why I add '+ 1' ..
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
