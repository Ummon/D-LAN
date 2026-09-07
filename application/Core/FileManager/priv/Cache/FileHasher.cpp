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
#include <QByteArray>
#include <QString>
#include <QFile>
#include <QElapsedTimer>
#include <QScopeGuard>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/Hash.h>

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

FileHasher::~FileHasher()
{
   this->flushHashes();
}

void FileHasher::flushHashes()
{
   QMutexLocker locker(&this->hashingMutex);
   this->flushPendingHashes();
}

void FileHasher::flushPendingHashes()
{
   for (const auto& chunk : std::as_const(this->pendingHashSaves))
      chunk->saveFileHashes();
   this->pendingHashSaves.clear();
}

qint64 FileHasher::read(QFile& file, char* data, qint64 maxSize)
{
   return file.read(data, maxSize);
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
  * @param deferPersistence Batch partial progress until flushHashes(); completed files and interrupted calls still save immediately.
  * @return true if all the chunk hashes are known.
  * @exception IOErrorException Thrown when the file cannot be opened or read. Some chunk may be computed before this exception is thrown.
  */
bool FileHasher::start(File* fileCache, int n, int* amountHashed, bool deferPersistence)
{
   QMutexLocker locker(&this->hashingMutex);
   bool deferSave = false;
   const auto saveProgress = qScopeGuard([&] {
      if (!deferSave)
         this->flushPendingHashes();
   });

   this->currentFileCache = fileCache;

   connect(
      this->currentFileCache->getCache(),
      &Cache::entryRemoved,
      this, &FileHasher::entryRemoved,
      static_cast<Qt::ConnectionType>(Qt::UniqueConnection | Qt::DirectConnection)
   );

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      this->currentFileCache = nullptr;
      return false;
   }

   // A scheduler may have selected this file before re-download began. Connect
   // removal above before checking: a transition after this check must stop us,
   // while a transition already in progress must not start another hashing pass.
   if (!this->currentFileCache->isComplete())
   {
      this->currentFileCache = nullptr;
      return false;
   }

   const QString filePath = this->currentFileCache->getAbsolutePath();
   const QFileInfo initialInfo(filePath);
   const auto restartWithNewChunks = [&]()
   {
      L_WARN(QString("The file changed during hashing, its chunks are reset: %1").arg(filePath));
      // Re-download may already be waiting for us to stop; it owns that transition.
      if (this->currentFileCache->isComplete())
         this->currentFileCache->fileHasChangedOnDisk(QFileInfo(filePath));
      this->toStopHashing = false;
      this->hashing = false;
      this->currentFileCache = nullptr;
      return false;
   };

   if (!initialInfo.exists())
   {
      this->currentFileCache = nullptr;
      throw IOErrorException();
   }

   // Previously computed (or loaded) hashes belong to the cached size and date.
   // Check before skipping any chunks, including when the chunk count is unchanged.
   if (!this->currentFileCache->correspondTo(initialInfo))
      return restartWithNewChunks();

   // An empty file has no chunk: there is nothing to hash and no file to open.
   const QList<QSharedPointer<Chunk>> chunks = this->currentFileCache->getChunks();
   if (chunks.isEmpty())
   {
      this->currentFileCache = nullptr;
      return true;
   }

   this->hashing = true;

   L_USER(tr("Computing hashes of %1 . . .").arg(filePath));

   // Same performance with or without "QIODevice::Unbuffered".
   AutoReleasedFile file(
      this->filePool,
      filePath,
      QIODevice::ReadOnly | QIODevice::Unbuffered,
      this->currentFileCache->getSize() <= Chunk::CHUNK_SIZE
   );

   if (!file || !file->reset())
   {
      this->toStopHashing = false;
      this->hashing = false;
      this->currentFileCache = 0;
      L_WARN(QString("Unable to open this file: %1").arg(filePath));
      throw IOErrorException();
   }

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

   const int BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
   QByteArray buffer(BUFFER_SIZE, Qt::Uninitialized);

   Common::Hasher hasher;
   bool endOfFile = false;
   qint64 bytesReadTotal = 0;
   struct ComputedHash
   {
      int num;
      Common::Hash hash;
      int knownBytes;
   };
   QList<ComputedHash> computedHashes;

   while (!endOfFile)
   {
      int bytesReadChunk = 0;
      while (bytesReadChunk < Chunk::CHUNK_SIZE)
      {
         // See 'stopHashing()'.
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
            // A buffer need not divide the chunk size (and may even exceed it). Never consume
            // bytes belonging to the next chunk when calculating this chunk's hash.
            bytesRead = this->read(*file, buffer.data(), qMin(BUFFER_SIZE, Chunk::CHUNK_SIZE - bytesReadChunk));
            switch (bytesRead)
            {
            case -1:
               this->toStopHashing = false;
               this->hashing = false;
               this->currentFileCache = 0;
               L_ERRO(QString("Error when reading the file %1").arg(filePath));
               throw IOErrorException();
            case 0:
               endOfFile = true;
               if (bytesReadChunk + bytesReadTotal + bytesSkipped != initialInfo.size())
                  return restartWithNewChunks();
               goto endReading;
            }
         }

         hasher.addData(std::span<const char>(buffer).first(static_cast<size_t>(bytesRead)));

         bytesReadChunk += bytesRead;
      }
      endReading:

      bytesReadTotal += bytesReadChunk;

      if (bytesReadChunk > 0)
      {
         if (chunkNum >= chunks.size())
            return restartWithNewChunks();

         if (amountHashed)
            *amountHashed += bytesReadChunk;

         // Keep this pass private: even a reader of Chunk::getHash() must not see
         // data that has not passed the final filesystem validation.
         computedHashes.append({ chunkNum, hasher.getResult(), bytesReadChunk });

         if (--n == 0)
            break;
      }

      hasher.reset();
      chunkNum += 1;
   }

#ifdef DEBUG
   const int delta = timer.elapsed();
   if (delta < 50)
      L_DEBU("Hashing speed: ?? MB/s (delta too small)");
   else
   {
      const int speed = 1000LL * bytesReadTotal / delta;
      L_DEBU(QString("Hashing speed: %1/s").arg(Common::Global::formatByteSize(speed)));
   }
#endif

   // Never attach a new timestamp to hashes computed from an older file version.
   // Validate partial passes as well as completion, before publication or persistence.
   const QFileInfo finalInfo(filePath);
   if (!finalInfo.exists() || finalInfo.size() != initialInfo.size() ||
       finalInfo.lastModified() != initialInfo.lastModified())
      return restartWithNewChunks();

   for (const auto& computed : computedHashes)
   {
      const auto& chunk = chunks[computed.num];
      const auto fileMutex = this->currentFileCache->getChunkMutex();
      QMutexLocker fileLocker(fileMutex.data());
      // A re-download or retirement can begin while disk reads are unlocked.
      if (!this->currentFileCache->isComplete() || !chunk->isOwnedBy(this->currentFileCache))
      {
         this->toStopHashing = false;
         this->hashing = false;
         this->currentFileCache = nullptr;
         return false;
      }
      if (chunk->getHash() != computed.hash)
      {
         if (chunk->hasHash())
            this->currentFileCache->getCache()->onChunkRemoved(chunk);
         chunk->setHash(computed.hash, false);
         chunk->setKnownBytes(computed.knownBytes);
         this->pendingHashSaves.insert(chunks.first());
         this->currentFileCache->getCache()->onChunkHashKnown(chunk);
      }
   }

   this->toStopHashing = false;
   this->hashing = false;

   qint64 fileSize = this->currentFileCache->getSize();
   const bool complete = bytesReadTotal + bytesSkipped == fileSize;
   if (complete && this->pendingHashSaves.remove(chunks.first()))
      chunks.first()->saveFileHashes();
   deferSave = deferPersistence;
   this->currentFileCache = 0;
   return complete;
}

void FileHasher::stop()
{
   QMutexLocker locker(&this->hashingMutex);
   this->internalStop();
   this->flushPendingHashes();
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
      L_DEBU(
         QString("FileHasher::stop(): %1 . . .")
            .arg(this->currentFileCache ? this->currentFileCache->getAbsolutePath().toString() : "?")
      );
      this->hashingStopped.wait(&this->hashingMutex);
      L_DEBU("File hashing stopped");
   }
}
