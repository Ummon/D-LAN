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

      Common::Hashes getHashes(const QString& filePath) override;
      void setHashes(QString& filePath, const QList<QString>& filePaths) override;
      void setSizeAndDateTime(QString& filePath, qint64 size, QDateTime dateTime) override;
      void rmHashes(QString& filePath) override;

   private:
      LOG_INIT_H("HashCache")

      QSqlDatabase db;
   };
}
