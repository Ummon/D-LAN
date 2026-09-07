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

#include <priv/GetHashesResult.h>

#include <QSharedPointer>
#include <QList>
#include <QMutexLocker>
#include <QMetaType>

#include <Protos/core_protocol.pb.h>

#include <priv/Cache/File.h>
#include <priv/Cache/Chunk.h>
#include <priv/Log.h>

using namespace FM;

GetHashesResult::GetHashesResult(const Protos::Common::Entry& fileEntry, Cache& cache, FileUpdater& fileUpdater) :
   fileEntry(fileEntry), cache(cache), fileUpdater(fileUpdater)
{
   qRegisterMetaType<Protos::Core::HashResult>("Protos::Core::HashResult");

   L_DEBU("GetHashesResult::GetHashesResult(..)");
}

GetHashesResult::~GetHashesResult()
{
   // After the 'emit nextHash(chunk->getHash());' the receiver (in another thread) can decide to clear the
   // QSharedPointer, if it does and it's the last reference the object will be destroyed by an another thread and
   // 'mutex' will be unlock by this other thread.
   QMutexLocker locker(&this->mutex);

   L_DEBU("GetHashesResult::~GetHashesResult()");

   this->disconnectFromCache();
}

/**
  * Called from the main thread.
  */
Protos::Core::GetHashesResult GetHashesResult::start()
{
   Protos::Core::GetHashesResult result;

   File* file = this->cache.getFile(this->fileEntry);
   if (!file)
   {
      result.set_status(Protos::Core::GetHashesResult_Status_DONT_HAVE);
      return result;
   }

   bool prioritize = false;
   {
      // Same order as chunk retirement/notification: file mutex, then result mutex.
      // Keep size validation, subscription and the initial snapshot in one generation.
      const auto fileMutex = file->getChunkMutex();
      QMutexLocker fileLocker(fileMutex.data());
      QMutexLocker locker(&this->mutex);
      this->chunks = file->getChunks();
      if (!file->isComplete() || this->fileEntry.size() != static_cast<quint64>(file->getSize()) ||
          this->fileEntry.chunks_size() != this->chunks.size())
      {
         result.set_status(Protos::Core::GetHashesResult_Status_ERROR_UNKNOWN);
         return result;
      }

      for (int i = 0; i < this->chunks.size(); ++i)
      {
         const auto hash = this->chunks[i]->getHash();
         const auto& requested = this->fileEntry.chunks(i).hash();
         if (!requested.empty() && !hash.isNull() &&
             requested != std::string(hash.getData(), Common::Hash::HASH_SIZE))
         {
            result.set_status(Protos::Core::GetHashesResult_Status_ERROR_UNKNOWN);
            return result;
         }
      }

      connect(&this->cache, &Cache::chunkHashKnown, this, &GetHashesResult::chunkHashKnown, Qt::DirectConnection);
      connect(&this->cache, &Cache::chunkRemoved, this, &GetHashesResult::chunkRemoved, Qt::DirectConnection);

      int nbOfHashToSend = 0;
      int j = 0;
      for (QListIterator<QSharedPointer<Chunk>> i(this->chunks); i.hasNext();)
      {
         auto chunk = i.next();
         const Protos::Common::Hash& protoChunk = this->fileEntry.chunks(j++);
         // Only for unknown hashes (size == 0).
         if (protoChunk.hash().size() == 0)
         {
            nbOfHashToSend++;
            if (chunk->hasHash())
               this->sendNextHash(chunk, true);
            else
               this->hashesRemaining << chunk->getNum();
         }
      }

      result.set_nb_hash(nbOfHashToSend);
      prioritize = !this->hashesRemaining.isEmpty();
      if (!prioritize)
         this->disconnectFromCache();
   }

   result.set_status(Protos::Core::GetHashesResult_Status_OK);

   // Never acquire the updater's scheduler mutex while holding a file mutex.
   if (prioritize)
      this->fileUpdater.prioritizeAFileToHash(file);

   return result;
}

void GetHashesResult::chunkHashKnown(QSharedPointer<Chunk> chunk)
{
   QMutexLocker locker(&this->mutex);
   if (!this->invalidated && this->ownsChunk(chunk))
      this->sendNextHash(chunk, false);
}

void GetHashesResult::chunkRemoved(QSharedPointer<Chunk> chunk)
{
   QMutexLocker locker(&this->mutex);
   if (this->ownsChunk(chunk))
   {
      // The streaming protocol has no cancellation message. Stop this stream;
      // its caller's existing timeout handles the incomplete request.
      this->invalidated = true;
      this->hashesRemaining.clear();
      this->disconnectFromCache();
   }
}

bool GetHashesResult::ownsChunk(const QSharedPointer<Chunk>& chunk) const
{
   const int number = chunk->getNum();
   return number >= 0 && number < this->chunks.size() && this->chunks[number] == chunk;
}

void GetHashesResult::disconnectFromCache()
{
   disconnect(&this->cache, &Cache::chunkHashKnown, this, &GetHashesResult::chunkHashKnown);
   disconnect(&this->cache, &Cache::chunkRemoved, this, &GetHashesResult::chunkRemoved);
}

void GetHashesResult::sendNextHash(QSharedPointer<Chunk> chunk, bool direct)
{
   if (!direct)
   {
      const int i = this->hashesRemaining.indexOf(chunk->getNum());
      // Notifications can repeat or concern hashes already supplied by the requester/sent by start().
      if (i == -1)
         return;

      this->hashesRemaining.removeAt(i);
      if (this->hashesRemaining.empty())
         this->disconnectFromCache();
   }

   Protos::Core::HashResult hashResult;
   hashResult.set_num(chunk->getNum());
   hashResult.mutable_hash()->set_hash(chunk->getHash().getData(), Common::Hash::HASH_SIZE);
   emit nextHash(hashResult);
}
