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
  
#include <priv/Cache/FileHasher.h>
using namespace FM;

#include <QMutexLocker>
#include <QString>
#include <QFile>
#include <QElapsedTimer>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/Hash.h>
#include <Common/FileLocker.h>

#include <Exceptions.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/File.h>
#include <priv/Log.h>

/**
  * @class FileHasher
  *
  * The class can compute the hashes of a given file (FM::File*).
  * A 'Chunk' object is added to the file for each hash computed.
  */

FileHasher::FileHasher() :
   currentFileCache(0),
   hashing(false),
   toStopHashing(false)
{
}

/**
  * It will open the file, read it and calculate all theirs chunk hashes.
  * Only the chunk without hashes will be computed.
  * This method can be called from an another thread than the main one. For example,
  * from 'FileUpdated' thread.
  *
  * @param fileCache The file to hash.
  * @param n Number of hashes to compute, 0 if we want to compute all the hashes.
  * @param[out] amountHashed Write the number of bytes hashed. It may be a null pointer ('nullptr') if this information isn't needed.
  * @exception IOErrorException Thrown when the file cannot be opened or read. Some chunk may be computed before this exception is thrown.
  */
bool FileHasher::start(FileForHasher* fileCache, int n, int* amountHashed)
{
   QMutexLocker locker(&this->hashingMutex);

   this->currentFileCache = fileCache;

   connect(this->currentFileCache->getCache(), SIGNAL(entryRemoved(Entry*)), this, SLOT(entryRemoved(Entry*)), Qt::UniqueConnection);

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      this->currentFileCache = 0;
      return false;
   }

   this->hashing = true;

   const QString& filePath = this->currentFileCache->getFullPath();

   L_USER(tr("Computing the hashes for %1 . . .").arg(filePath));

   // Same performance with or without "QIODevice::Unbuffered".
   AutoReleasedFile file(FileHasher::filePool, filePath, QIODevice::ReadOnly | QIODevice::Unbuffered, this->currentFileCache->getSize() <= Chunk::CHUNK_SIZE);

   if (!file)
   {
      this->toStopHashing = false;
      this->hashing = false;
      this->currentFileCache = 0;
      L_WARN(QString("Unable to open this file : %1").arg(filePath));
      throw IOErrorException();
   }

   file->reset();

   const QVector<QSharedPointer<Chunk>>& chunks = this->currentFileCache->getChunks();

   // Skip the already known full hashes.
   qint64 bytesSkipped = 0;
   int chunkNum = 0;
   while (
      chunkNum < chunks.size() &&
      chunks[chunkNum]->hasHash() &&
      chunks[chunkNum]->getKnownBytes() == Chunk::CHUNK_SIZE) // Maybe the file has grown and the last chunk must be recomputed.
   {
      bytesSkipped += Chunk::CHUNK_SIZE;
      chunkNum++;
      file->seek(file->pos() + Chunk::CHUNK_SIZE);
   }

#if DEBUG
   QElapsedTimer timer;
   timer.start();
#endif

   static const int BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
   char buffer[BUFFER_SIZE];

   Common::Hasher hasher;
   bool endOfFile = false;
   qint64 bytesReadTotal = 0;

   while (!endOfFile)
   {
      // See 'stopHashing()'.

      int bytesReadChunk = 0;
      while (bytesReadChunk < Chunk::CHUNK_SIZE)
      {
         locker.unlock();
         locker.relock();

         if (this->toStopHashing)
         {
            this->hashingStopped.wakeOne();
            this->toStopHashing = false;
            this->hashing = false;
            this->currentFileCache = 0;
            return false;
         }

         int bytesRead = 0;
         {
            Common::FileLocker fileLocker(*file, BUFFER_SIZE, Common::FileLocker::READ);
            if (!fileLocker.isLocked())
            {
               this->toStopHashing = false;
               this->hashing = false;
               this->currentFileCache = 0;
               L_WARN(QString("Unable to acquire the lock for this file : %1").arg(filePath));
               throw IOErrorException();
            }

            bytesRead = file->read(buffer, BUFFER_SIZE);
            switch (bytesRead)
            {
            case -1:
               this->toStopHashing = false;
               this->hashing = false;
               this->currentFileCache = 0;
               L_ERRO(QString("Error during reading the file %1").arg(filePath));
               throw IOErrorException();
            case 0:
               endOfFile = true;
               this->currentFileCache->setSize(bytesReadChunk + bytesReadTotal + bytesSkipped);
               goto endReading;
            }
         }

         hasher.addData(buffer, bytesRead);

         bytesReadChunk += bytesRead;
      }
      endReading:

      bytesReadTotal += bytesReadChunk;

      if (bytesReadChunk > 0)
      {
         if (amountHashed)
            *amountHashed += bytesReadChunk;

         const Common::Hash& hash = hasher.getResult();

         if (chunks.size() <= chunkNum) // The size of the file has increased during the read . . .
         {
            QSharedPointer<Chunk> newChunk(new Chunk(this->currentFileCache, chunkNum, bytesReadChunk, hash));
            this->currentFileCache->addChunk(newChunk);
            this->currentFileCache->getCache()->onChunkHashKnown(newChunk);
         }
         else
         {
            if (chunks[chunkNum]->getHash() != hash)
            {
               if (chunks[chunkNum]->hasHash())
                  this->currentFileCache->getCache()->onChunkRemoved(chunks[chunkNum]); // To remove the chunk from the chunk index (TODO: find a more elegant way).

               chunks[chunkNum]->setHash(hash);
               chunks[chunkNum]->setKnownBytes(bytesReadChunk);

               this->currentFileCache->getCache()->onChunkHashKnown(chunks[chunkNum]);
            }
         }

         if (--n == 0)
            break;
      }

      hasher.reset();
      chunkNum += 1;
   }

#ifdef DEBUG
   const int delta = timer.elapsed();
   if (delta < 50)
      L_DEBU("Hashing speed : ?? MB/s (delta too small)");
   else
   {
      const int speed = 1000LL * bytesReadTotal / delta;
      L_DEBU(QString("Hashing speed : %1/s").arg(Common::Global::formatByteSize(speed)));
   }
#endif

   this->toStopHashing = false;
   this->hashing = false;

   // TODO: seriously rethink this part, a file being written shouldn't be shared or hashed . . .
   if (bytesReadTotal + bytesSkipped != this->currentFileCache->getSize())
   {
      if (n != 0)
      {
         L_DEBU(QString("The file content has changed during the hashes computing process. File = %1, bytes read = %2, previous size = %3").arg(filePath).arg(bytesReadTotal).arg(this->currentFileCache->getSize()));
         this->currentFileCache->setSize(bytesReadTotal + bytesSkipped);
         this->currentFileCache->updateDateLastModified(QFileInfo(filePath).lastModified());

         if (bytesReadTotal + bytesSkipped < this->currentFileCache->getSize()) // In this case, maybe some chunk must be deleted.
            for (int i = this->currentFileCache->getNbChunks(); i < chunks.size(); i++)
            {
               QSharedPointer<Chunk> c = this->currentFileCache->removeLastChunk();
               this->currentFileCache->getCache()->onChunkRemoved(c);
            }
      }
      this->currentFileCache = 0;
      return false;

   }

   this->currentFileCache->updateDateLastModified(QFileInfo(filePath).lastModified()); // A file may have been changed from its creation in the cache.
   this->currentFileCache = 0;
   return true;
}

void FileHasher::stop()
{
   QMutexLocker locker(&this->hashingMutex);
   this->internalStop();
}

void FileHasher::entryRemoved(Entry* entry)
{
   QMutexLocker locker(&this->hashingMutex);
   if (this->currentFileCache == entry)
      this->internalStop();
}

void FileHasher::internalStop()
{
   this->toStopHashing = true;
   if (this->hashing)
   {
      L_DEBU(QString("FileHasher::stop(): %1 ..").arg(this->currentFileCache ? this->currentFileCache->getFullPath() : "?"));
      this->hashingStopped.wait(&this->hashingMutex);
      L_DEBU("File hashing stopped");
   }
}

FilePool FileHasher::filePool;
