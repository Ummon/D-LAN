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
