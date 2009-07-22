#include "File.h"

#include <QDebug>
#include <QMutexLocker>

#include <Chunk.h>

File::File(const QString& name, quint64 size) throw(FileSizeDoesntMatchException)
      : size(size)
{
   this->fileInWriteMode = new QFile(name);
   if (this->fileInWriteMode->exists())
   {
      if ((quint64)this->fileInWriteMode->size() != this->size)
         throw FileSizeDoesntMatchException();
   }
   this->fileInWriteMode->open(QIODevice::WriteOnly);
   this->fileInWriteMode->resize(this->size); // Warning : can fail

   this->fileInReadMode = new QFile(name);
   this->fileInReadMode->open(QIODevice::ReadOnly);
      
   quint64 offset = 0;
   for (int chunkNum = 0; offset < size; offset += File::chunkSize, chunkNum++)
      this->chunks.append(new Chunk(*this, chunkNum));
}

File::~File()
{
   this->fileInWriteMode->flush();
   
   delete this->fileInWriteMode;
   delete this->fileInReadMode;
   foreach (Chunk* chunk, this->chunks)
      delete chunk;
}

bool File::write(const QByteArray& buffer, qint64 offset)
{
   if ((quint64)offset >= this->size)
      return true;
   
   QMutexLocker locker(&this->writeLock);
   
   this->fileInWriteMode->seek(offset);
   int maxSize = (this->size - offset);
   qint64 n = this->fileInWriteMode->write(buffer.data(), buffer.size() > maxSize ? maxSize : buffer.size());
   
   return offset + n >= (qint64)this->size || n == -1;
}

qint64 File::read(QByteArray& buffer, qint64 offset)
{
   if ((quint64)offset >= this->size)
      return 0;

   QMutexLocker locker(&this->readLock);

   this->fileInReadMode->seek(offset);
   qint64 bytesRead = this->fileInReadMode->read(buffer.data(), buffer.size());
   
   return bytesRead;
}
   
const QList<Chunk*>& File::getChunks()
{
   return this->chunks;
}
