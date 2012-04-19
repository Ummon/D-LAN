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
  * - remove 'a'. 'b' wouldn't be remove from Chunks in the same time.
  */

void Chunks::add(QSharedPointer<Chunk> chunk)
{
   QMutexLocker locker(&this->mutex);
   this->insert(chunk->getHash(), chunk);
}

void Chunks::rm(QSharedPointer<Chunk> chunk)
{
   QMutexLocker locker(&this->mutex);
   this->remove(chunk->getHash(), chunk);
   L_DEBU(QString("Nb chunks: %1").arg(this->size()));
}

/**
  * If there is some identical chunks (same data, same hash) it always returns the same (first) chunk.
  * The goal is to optimize the downloading of file with a lot of padding like a Wii ISO game. In this case many chunks are identical
  * and may be copied from the local disk, the data can be cached in a better way by the OS if it copies always from the same chunk.
  *
  * Some measurements for downloading a file with 60% of padding on Windows 7:
  *  - Last inserted chunk returned: 2 min 35 s
  *  - First inserted chunk returned: 1 min 40 s
  */
QSharedPointer<Chunk> Chunks::value(const Common::Hash& hash) const
{
   QMutexLocker locker(&this->mutex);

   QHash<Common::Hash, QSharedPointer<Chunk>>::const_iterator i = QMultiHash<Common::Hash, QSharedPointer<Chunk>>::constFind(hash);
   if (i == QHash<Common::Hash, QSharedPointer<Chunk>>::constEnd())
      return QSharedPointer<Chunk>();
   else
      return i.value();
}

QList<QSharedPointer<Chunk>> Chunks::values(const Common::Hash& hash) const
{
   QMutexLocker locker(&this->mutex);
   QList<QSharedPointer<Chunk>> values;

   QMultiHash<Common::Hash, QSharedPointer<Chunk>>::const_iterator i = this->find(hash);
   while (i != this->end() && i.key() == hash)
   {
      values << i.value();
      ++i;
   }

   return values;
}

bool Chunks::contains(const Common::Hash& hash) const
{
   QMutexLocker locker(&this->mutex);
   return QMultiHash<Common::Hash, QSharedPointer<Chunk>>::contains(hash);
}
