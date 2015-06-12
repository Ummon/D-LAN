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
  
#include <priv/ChunkIndex/Chunks.h>
using namespace FM;

#include <priv/Cache/Chunk.h>
#include <priv/Log.h>

/**
  * @class FM::Chunks
  *
  * We must allow multiple chunk with the same hash. Considering this case :
  * - Add identical files 'a' and 'b'.
  * - remove 'a'. 'b' wouldn't be remove from Chunks at the same time.
  *
  * We may use a Bloom filter to reduce the time of a call to 'contains(..)', 'value(..)' and 'values(..)'.
  * Some measurements (compiled with GCC 4.6 and -02):
  *  - The filter reduces the call time of 'contains()' from about 20% with 30'000 hashes
  *    and about 15% with 100'000 hashes.
  *  - The filter increases the call time of 'contains()' from about 100% with 1'000'000 hashes.
  *  - 100'000'000 calls of 'contains(..)' for 100'000 known chunks takes 3.6s on a i5 @ 2.5 GHz.
  *    It's 36 ns per call.
  * See the method 'chunksPerformance()' in 'TestsFileManager' for more information.
  */

void Chunks::add(const QSharedPointer<Chunk>& chunk)
{
   if (chunk->getHash().isNull())
      return;

   QMutexLocker locker(&this->mutex);
   this->insert(chunk->getHash(), chunk);
#ifdef BLOOM_FILTER_ON
   this->bloomFilter.add(chunk->getHash());
#endif
}

void Chunks::rm(const QSharedPointer<Chunk>& chunk)
{
   if (chunk->getHash().isNull())
      return;

   QMutexLocker locker(&this->mutex);
   this->remove(chunk->getHash(), chunk);
#ifdef BLOOM_FILTER_ON
   if (this->isEmpty())
      this->bloomFilter.reset();
#endif
   // L_DEBU(QString("Nb chunks: %1").arg(this->size()));
}

QSharedPointer<Chunk> Chunks::value(const Common::Hash& hash) const
{
   if (hash.isNull())
      return QSharedPointer<Chunk>();

   QMutexLocker locker(&this->mutex);
#ifdef BLOOM_FILTER_ON
   if (!this->bloomFilter.test(hash))
      return QSharedPointer<Chunk>();
#endif
   return QMultiHash<Common::Hash, QSharedPointer<Chunk>>::value(hash);
}

QList<QSharedPointer<Chunk>> Chunks::values(const Common::Hash& hash) const
{
   if (hash.isNull())
      return QList<QSharedPointer<Chunk>>();

   QMutexLocker locker(&this->mutex);
#ifdef BLOOM_FILTER_ON
   if (!this->bloomFilter.test(hash))
      return QList<QSharedPointer<Chunk>>();
#endif
   return QMultiHash<Common::Hash, QSharedPointer<Chunk>>::values(hash);
}

bool Chunks::contains(const Common::Hash& hash) const
{
   if (hash.isNull())
      return false;

   QMutexLocker locker(&this->mutex);   
#ifdef BLOOM_FILTER_ON
   if (!this->bloomFilter.test(hash))
      return false;
#endif
   return QMultiHash<Common::Hash, QSharedPointer<Chunk>>::contains(hash);
}
