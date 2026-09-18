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

#include <memory>

#include <QTimer>
#include <QString>
#include <QThread>

#include <Common/Hash.h>
#include <Common/Path.h>

#include <IHashCache.h>
#include <priv/Constants.h>

class Tests;

namespace HC
{
   class HashCache : public IHashCache
   {
   public:
      HashCache(const QString& databaseFolder, int initialFileCheckDelay = INITIAL_FILE_CHECK_DELAY);
      ~HashCache();

      QList<Common::Hash> getHashes(const QString& filePath, qint64 size, QDateTime timeLastModified = QDateTime()) override;
      QList<QList<Common::Hash>> getHashesBatch(const QList<FileMetadata>& files) override;

      void setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime = QDateTime()) override;

      void rmHashes(const QString& filePath) override;

   private:
      friend class ::Tests; // Drive individual maintenance batches deterministically.
      class Database;
      void flushPendingHashes(); // Called only on databaseThread.
      int checkFiles(); // Called only on databaseThread; returns the next timer delay.

      // SQL objects and the maintenance timer belong to databaseThread.
      // Reads block for their result; writes own their arguments and are queued.
      QThread databaseThread;
      QObject* databaseContext;
      std::unique_ptr<Database> database;

      QTimer* checkDeletedFileTimer = nullptr;
      QTimer* flushHashesTimer = nullptr;
   };
}
