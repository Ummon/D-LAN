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
  
#pragma once

#include <QObject>
#include <QPointer>
#include <QBitArray>

#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>

#include <IGetHashesResult.h>
#include <priv/Cache/Cache.h>
#include <priv/FileUpdater/FileUpdater.h>

namespace FM
{
   class Cache;
   class File;
   class FileUpdater;

   class GetHashesResult : public IGetHashesResult, Common::Uncopyable
   {
      Q_OBJECT
   public:
      GetHashesResult(const Protos::Common::Entry& fileEntry, Cache& cache, FileUpdater& fileUpdater);
      ~GetHashesResult();
      Protos::Core::GetHashesResult start() override;

   private slots:
      void chunkHashKnown(QSharedPointer<FM::Chunk> chunk);
      void chunkRemoved(QSharedPointer<FM::Chunk> chunk);
      void invalidate();

   private:
      void disconnectFromCache();
      bool ownsChunk(const QSharedPointer<Chunk>& chunk) const;

      const Protos::Common::Entry fileEntry;
      // Retain the original generation; pointer identity cannot be reused after retirement.
      QList<QSharedPointer<Chunk>> chunks;
      enum class State { Created, Streaming, Finished, Invalidated, Failed };
      State state = State::Created;
      Protos::Core::GetHashesResult startResult;
      QPointer<Cache> cache;
      QPointer<FileUpdater> fileUpdater;
      File* file = nullptr; // Identity only after start(); retained chunks protect against reuse.

      QBitArray pendingHashes;
      int hashesRemaining = 0;
   };
}
