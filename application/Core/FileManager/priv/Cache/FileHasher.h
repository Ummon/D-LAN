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
#include <QMutex>
#include <QWaitCondition>
#include <QSet>
#include <QSharedPointer>

#include <Common/Uncopyable.h>

#include <priv/Cache/FilePool.h>

namespace FM
{
   class Entry;
   class File;
   class Chunk;

   class FileHasher : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      FileHasher();
      ~FileHasher() override;

      bool start(File* fileCache, int n = 0, int* amountHashed = nullptr, bool deferPersistence = false);
      void flushHashes();
      void stop();

   private slots:
      void entryRemoved(FM::Entry* entry);

   private:
      void internalStop();
      void flushPendingHashes();

      File* currentFileCache;
      // One retained chunk per changed file generation; detachment makes deferred saves harmless.
      QSet<QSharedPointer<Chunk>> pendingHashSaves;

      bool hashing;
      bool toStopHashing;
      QWaitCondition hashingStopped;
      QMutex hashingMutex;

      FilePool filePool;
   };
}
