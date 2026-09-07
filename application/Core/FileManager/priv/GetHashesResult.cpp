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
#include <QThread>

#include <Protos/core_protocol.pb.h>

#include <priv/Cache/File.h>
#include <priv/Cache/Chunk.h>
#include <priv/Log.h>

using namespace FM;

GetHashesResult::GetHashesResult(const Protos::Common::Entry& fileEntry, Cache& cache, FileUpdater& fileUpdater) :
   fileEntry(fileEntry), cache(&cache), fileUpdater(&fileUpdater)
{
   Q_ASSERT(this->thread() == cache.thread());
   Q_ASSERT(this->thread() == fileUpdater.thread());
   qRegisterMetaType<Protos::Core::HashResult>("Protos::Core::HashResult");
   L_DEBU("GetHashesResult::GetHashesResult(..)");
}

GetHashesResult::~GetHashesResult()
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   this->disconnectFromCache();
   L_DEBU("GetHashesResult::~GetHashesResult()");
}

Protos::Core::GetHashesResult GetHashesResult::start()
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   if (this->state != State::Created)
      return this->startResult;

   this->state = State::Failed;
   this->startResult.set_status(Protos::Core::GetHashesResult_Status_DONT_HAVE);
   this->file = this->cache ? this->cache->getFile(this->fileEntry) : nullptr;
   if (!this->file || !this->fileUpdater)
      return this->startResult;

   QList<QSharedPointer<Chunk>> ready;
   bool prioritize = false;
   {
      // Validate and subscribe to one generation. Worker notifications are queued
      // onto our thread, so request state never needs a mutex.
      const auto fileMutex = this->file->getChunkMutex();
      QMutexLocker fileLocker(fileMutex.data());
      this->chunks = this->file->getChunks();
      this->startResult.set_status(Protos::Core::GetHashesResult_Status_ERROR_UNKNOWN);
      if (!this->file->isComplete() || this->fileEntry.size() != static_cast<quint64>(this->file->getSize()) ||
          this->fileEntry.chunks_size() != this->chunks.size())
         return this->startResult;

      for (int i = 0; i < this->chunks.size(); ++i)
      {
         const auto hash = this->chunks[i]->getHash();
         const auto& requested = this->fileEntry.chunks(i).hash();
         if (!requested.empty() && !hash.isNull() &&
             requested != std::string(hash.getData(), Common::Hash::HASH_SIZE))
            return this->startResult;
      }

      connect(this->cache, &Cache::chunkHashKnown, this, &GetHashesResult::chunkHashKnown);
      connect(this->cache, &Cache::chunkRemoved, this, &GetHashesResult::chunkRemoved);
      connect(this->cache, &QObject::destroyed, this, &GetHashesResult::invalidate);

      for (int i = 0; i < this->chunks.size(); ++i)
         if (this->fileEntry.chunks(i).hash().empty())
         {
            this->hashesRemaining << i;
            if (this->chunks[i]->hasHash())
               ready << this->chunks[i];
            else
               prioritize = true;
         }

      this->startResult.set_status(Protos::Core::GetHashesResult_Status_OK);
      this->startResult.set_nb_hash(this->hashesRemaining.size());
      this->state = this->hashesRemaining.isEmpty() ? State::Finished : State::Streaming;
      if (this->state == State::Finished)
         this->disconnectFromCache();
   }

   // Finish setup before invoking any receiver. A receiver may reenter start()
   // or release the last owner of this request on any notification.
   const auto result = this->startResult;
   if (prioritize)
      this->fileUpdater->prioritizeAFileToHash(this->file);

   const QPointer<GetHashesResult> guard(this);
   for (const auto& chunk : ready)
   {
      if (!guard)
         break;
      guard->chunkHashKnown(chunk);
   }
   return result;
}

void GetHashesResult::chunkHashKnown(QSharedPointer<Chunk> chunk)
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   if (this->state != State::Streaming || !this->ownsChunk(chunk))
      return;
   // A worker notification can be queued before the generation is retired.
   if (!chunk->isOwnedBy(this->file))
   {
      this->invalidate();
      return;
   }
   const auto hash = chunk->getHash();
   if (hash.isNull() || !this->hashesRemaining.removeOne(chunk->getNum()))
      return;
   if (this->hashesRemaining.isEmpty())
   {
      this->state = State::Finished;
      this->disconnectFromCache();
   }

   Protos::Core::HashResult result;
   result.set_num(chunk->getNum());
   result.mutable_hash()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   // All state changes are complete. Do not access this after a receiver runs.
   emit nextHash(result);
}

void GetHashesResult::chunkRemoved(QSharedPointer<Chunk> chunk)
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   if (this->ownsChunk(chunk))
      this->invalidate();
}

void GetHashesResult::invalidate()
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   if (this->state != State::Streaming)
      return;
   // The streaming protocol has no cancellation message; the caller's timeout
   // handles an incomplete request. Queued notifications must stay silent.
   this->state = State::Invalidated;
   this->hashesRemaining.clear();
   this->disconnectFromCache();
}

bool GetHashesResult::ownsChunk(const QSharedPointer<Chunk>& chunk) const
{
   const int number = chunk->getNum();
   return number >= 0 && number < this->chunks.size() && this->chunks[number] == chunk;
}

void GetHashesResult::disconnectFromCache()
{
   if (this->cache)
      disconnect(this->cache, nullptr, this, nullptr);
}
