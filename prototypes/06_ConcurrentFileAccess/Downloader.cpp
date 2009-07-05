#include <Downloader.h>

#include <Chunk.h>
#include <File.h>

#include <QDebug>
#include <QByteArray>

Downloader::Downloader()
      : chunk(0)
{
}

void Downloader::setChunk(Chunk* chunk)
{
   this->chunk = chunk;
}

void Downloader::run()
{      
   if (!this->chunk)
   {
      qDebug() << "A downloader has no chunk defined";
      return;
   }
   
   QByteArray buffer(Downloader::bufferSize, 0);
   Downloader::fillBuffer(buffer);
   
   uint offset = 0;
   while (offset < File::chunkSize)
   {
      qDebug() << "Downloader " << this->chunk->getNum() << " : writes some bytes " << hex << offset << " -> " << offset + Downloader::bufferSize-1;
      this->chunk->write(buffer, offset);
      offset += Downloader::bufferSize;
   }
   
   qDebug() << this->chunk->getNum() << " : finished";
}

void Downloader::fillBuffer(QByteArray& buffer)
{
   for (int i = 0; i < buffer.size(); i++)
      buffer[i] = (char)(i % 256);

}
const uint Downloader::bufferSize = 65536; // 64 kB
