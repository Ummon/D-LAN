#include "Uploader.h"

#include <QDebug>

#include <Downloader.h>

Uploader::Uploader()
      : chunk(0)
{
   if  (Uploader::refArray.size() == 0)
   {  
      Uploader::refArray.resize(Downloader::bufferSize);
      Downloader::fillBuffer(Uploader::refArray);
   }
}

void Uploader::setChunk(Chunk* chunk)
{
   this->chunk = chunk;
}

void Uploader::run()
{      
   if (!this->chunk)
   {
      qDebug() << "An uploader has no chunk defined";
      return;
   }
   
   // Active wait (very bad). There will no need of this in the final application,
   // a chunk is uploaded only if it is complete.
   while (!this->chunk->isComplete())
      this->usleep(20000); // 20 ms
   
   QByteArray buffer(Downloader::bufferSize, 0);
   
   uint offset = 0;
   while (offset < File::chunkSize)
   {
      buffer.fill(0);
      qDebug() << "Uploader " << this->chunk->getNum() << " : reads some bytes " << hex << offset << " -> " << offset + Downloader::bufferSize-1;
      qint64 bytesRead = this->chunk->read(buffer, offset);
      buffer.resize(bytesRead);
      
      if (Uploader::refArray.startsWith(buffer))
         qDebug() << "Uploader " << this->chunk->getNum() << " : read OK";
      else
         qDebug() << "Uploader " << this->chunk->getNum() << " : read KO";
      
      if (bytesRead < buffer.size())
         break;
      
      offset += Downloader::bufferSize;
   }
   
   qDebug() << this->chunk->getNum() << " : finished";

}

QByteArray Uploader::refArray;
      
