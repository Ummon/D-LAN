#include <ZeroCopyStreamQIODevice.h>
using namespace Common;

ZeroCopyOutputStreamQIODevice::ZeroCopyOutputStreamQIODevice(QIODevice* device)
   : device(device), bufferAvaible(false), bufferSize(PROTOBUF_STREAMING_BUFFER_SIZE), bytesWritten(0)
{
}


bool ZeroCopyOutputStreamQIODevice::Next(void** data, int* size)
{
   if (this->bufferAvaible)
   {
      int nBytes = this->device->write(this->buffer, this->bufferSize);
      if (nBytes == -1)
         return false;
      this->bytesWritten += nBytes;
   }

   this->bufferAvaible = true;
   *data = this->buffer;
   *size = this->bufferSize;

   return true;
}

void ZeroCopyOutputStreamQIODevice::BackUp(int count)
{
   if (this->bufferAvaible)
   {
      int nBytes = this->device->write(this->buffer, this->bufferSize - count);
      if (nBytes != -1)
         this->bytesWritten += count;
   }
}

google::protobuf::int64 ZeroCopyOutputStreamQIODevice::ByteCount() const
{
   return this->bytesWritten;
}


ZeroCopyInputStreamQIODevice::ZeroCopyInputStreamQIODevice(QIODevice* device)
   : device(device), backupCount(0), nbLastRead(0), bufferSize(PROTOBUF_STREAMING_BUFFER_SIZE), bytesRead(0)
{
}

bool ZeroCopyInputStreamQIODevice::Next(const void** data, int* size)
{
   if (this->backupCount)
   {
      *data = this->buffer + this->nbLastRead - this->backupCount;
      *size = this->nbLastRead;
      this->backupCount = 0;
      return true;
   }

   this->nbLastRead = this->device->read(this->buffer, this->bufferSize);
   if (this->nbLastRead <= 0)
      return false;
   this->bytesRead += this->nbLastRead;

   *data = this->buffer;
   *size = this->nbLastRead;

   return true;
}

void ZeroCopyInputStreamQIODevice::BackUp(int count)
{
   this->backupCount = count;
}

bool ZeroCopyInputStreamQIODevice::Skip(int count)
{
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
