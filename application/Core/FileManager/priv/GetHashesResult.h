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
  
#ifndef GETHASHESRESULT_H
#define GETHASHESRESULT_H

#include <QObject>
#include <QMutex>

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
      Protos::Core::GetHashesResult start();

   private slots:
      void chunkHashKnown(QSharedPointer<Chunk> chunk);

   private:
      void sendNextHash(QSharedPointer<Chunk> chunk);

      const Protos::Common::Entry fileEntry;
      File* file; // TODO: if the file is deleted how can we know, is it important?
      Cache& cache;
      FileUpdater& fileUpdater;

      QMutex mutex;
      int nbHash;
      int lastHashNumSent;
   };
}

#endif
