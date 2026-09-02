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

#include <QString>
#include <QSqlQuery>
#include <QSqlDatabase>
#include <QTimer>
#include <QMutex>

#include <Common/Hash.h>
#include <Common/Path.h>

#include <IHashCache.h>
#include <priv/Log.h>

namespace HC
{
   class HashCache : public IHashCache
   {
   public:
      HashCache(const QString& databaseFolder);
      ~HashCache();

      QList<Common::Hash> getHashes(const QString& filePath, QDateTime timeLastModified = QDateTime()) override;

      void setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime = QDateTime()) override;

      void rmHashes(const QString& filePath) override;

   private:
      LOG_INIT_H("HashCache")

      void updateDatabaseScheme();
      bool updateToNextVersion(int currentVersion);

      QSqlDatabase db;

      QSqlQuery queryGetHashesWithDate;
      QSqlQuery queryGetHashes;
      QSqlQuery querySetHashes;
      QSqlQuery queryRemoveHashes;

      QMutex mutex;

      static const QStringList VERSION_1;
   };
}
