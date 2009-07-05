#include "File.h"

#include <QDebug>

#include <Chunk.h>

File::File(const QString& name, quint64 size) throw(FileSizeDoesntMatch)
      : size(size)
{
   this->file = new QFile(name);
   if (this->file->exists())
   {
      if ((quint64)this->file->size() != this->size)
         throw FileSizeDoesntMatch();
   }
   this->file->open(QIODevice::ReadWrite);
   this->file->resize(this->size); // Warning : can fail
      
   quint64 offset = 0;
   for (int chunkNum = 0; offset < size; offset += File::chunkSize, chunkNum++)
      this->chunks.append(new Chunk(*this, chunkNum));
}

File::~File()
{
   this->file->flush();
   delete this->file;
}

bool File::write(const QByteArray& buffer, qint64 offset)
{
   if ((quint64)offset >= this->size)
      return true;
   
   this->lock.lockForWrite();
   this->file->seek(offset);
   int maxSize = (this->size - offset);
   qint64 n = this->file->write(buffer.data(), buffer.size() > maxSize ? maxSize : buffer.size());
   this->lock.unlock();
   
   return offset + n >= (qint64)this->size || n == -1;
}

qint64 File::read(QByteArray& buffer, qint64 offset)
{
   if ((quint64)offset >= this->size)
      return 0;
   
   this->lock.lockForRead();
   this->file->seek(offset);
   qint64 bytesRead = this->file->read(buffer.data(), buffer.size());
   this->lock.unlock();
   
   return bytesRead;
}
   
const QList<Chunk*>& File::getChunks()
{
   return this->chunks;
}
