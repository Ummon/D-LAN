#pragma once

#include <QString>
#include <QSqlDatabase>

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

      QList<Common::Hash> getHashes(const QString& filePath, QDateTime timeLastModified = QDateTime()) override;

      void setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime = QDateTime()) override;

      // void setSizeAndDateTime(QString& filePath, qint64 size, QDateTime dateTime) override;

      void rmHashes(const QString& filePath) override;

   private:
      LOG_INIT_H("HashCache")

      void updateDatabaseScheme();
      bool updateToNextVersion(int currentVersion);

      QSqlDatabase db;

      static const QStringList VERSION_1;
   };
}
