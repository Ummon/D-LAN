#include <priv/HashCache.h>
using namespace HC;

#include <QDir>
#include <QSqlError>

#include <Common/Global.h>
#include <Common/Hash.h>
#include <Common/Constants.h>

#include <priv/Log.h>
#include <priv/Exceptions.h>

LOG_INIT_CPP(HashCache)

HashCache::HashCache(const QString& databaseFolder) :
   db { QSqlDatabase::addDatabase("QSQLITE") },
   queryGetHashesWithDate(this->db),
   queryGetHashes(this->db),
   querySetHashes(this->db),
   queryRemoveHashes(this->db)
{
   QMutexLocker locker(&this->mutex);

   const QString DATABASE_FILEPATH = QString("%1/%2").arg(databaseFolder, Common::Constants::HASH_CACHE_INDEX_FILENAME);
   L_DEBU(QString("HashCashe database: %1").arg(DATABASE_FILEPATH));

   this->db.setDatabaseName(DATABASE_FILEPATH);

   if (!this->db.open()) {
      L_ERRO(QString("Unable to open hash cache index database: %1").arg(db.lastError().text()));
   }

   QSqlQuery query(this->db);
   query.exec("PRAGMA foreign_keys = ON");
   query.exec("PRAGMA journal_mode = WAL");
   query.exec("PRAGMA synchronous = NORMAL");

   this->updateDatabaseScheme();

   this->queryGetHashesWithDate.prepare(
      "SELECT [hashes], [size] FROM [File] WHERE [path] = $1 AND [date_last_modified] = $2"
   );

   this->queryGetHashes.prepare("SELECT [hashes], [size] FROM [File] WHERE [path] = $1");

   this->querySetHashes.prepare(
      R"(
INSERT INTO [File] ([path], [size], [date_last_modified], [hashes])
VALUES ($1, $2, $3, $4)
ON CONFLICT([path]) DO
UPDATE SET [path] = $1, [size] = $2, [date_last_modified] = $3, [hashes] = $4
      )"
   );

   this->queryRemoveHashes.prepare("DELETE FROM [File] WHERE [path] = $1");
}

HashCache::~HashCache()
{
   this->db.close();
   L_DEBU("HashCache deleted");
}

QList<Common::Hash> HashCache::getHashes(const QString& filePath, QDateTime timeLastModified)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("[getHashes] filePath: %1").arg(filePath));

   QSqlQuery& query = timeLastModified.isNull() ? this->queryGetHashes : this->queryGetHashesWithDate;
   query.bindValue(0, filePath);

   if (!timeLastModified.isNull())
      query.bindValue(1, timeLastModified.toMSecsSinceEpoch());

   query.exec();

   if (!query.isActive())
   {
      L_ERRO(QString("[getHashes] SQL Error: %1").arg(query.lastError().text()));
      query.finish();
      return QList<Common::Hash>();
   }

   if (query.first())
   {
      const QByteArray hashes = query.value(0).toByteArray();
      const qint64 size = query.value(1).toLongLong();
      const int nbHashes = Common::Global::nbChunks(size);

      if (hashes.size() % Common::Hash::HASH_SIZE != 0 || hashes.size() / Common::Hash::HASH_SIZE != nbHashes)
      {
         query.finish();
         return QList<Common::Hash>();
      }

      QList<Common::Hash> result(nbHashes, Qt::Uninitialized);

      for (int i = 0; i < nbHashes; ++i)
         result[i] = Common::Hash(hashes.constData() + i * Common::Hash::HASH_SIZE);

      query.finish();
      return result;
   }

   query.finish();
   return QList<Common::Hash>();
}

void HashCache::setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("[setHashes] filePath: %1").arg(filePath));

   QByteArray hashesBlob;
   hashesBlob.reserve(hashes.size() * Common::Hash::HASH_SIZE);
   for (int i = 0; i < hashes.size(); ++i)
   {
      hashesBlob.append(hashes[i].getData(), Common::Hash::HASH_SIZE);
   }

   QSqlQuery& query = this->querySetHashes;

   query.bindValue(0, filePath);
   query.bindValue(1, size);
   query.bindValue(2, dateTime.isNull() ? 0 : dateTime.toMSecsSinceEpoch());
   query.bindValue(3, hashesBlob);
   query.exec();

   if (!query.isActive())
      L_ERRO(QString("[setHashes] SQL Error: %1").arg(query.lastError().text()));

   query.finish();
}

void HashCache::rmHashes(const QString& filePath)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("[rmHashes] filePath: %1").arg(filePath));

   QSqlQuery& query = this->queryRemoveHashes;

   query.bindValue(0, filePath);
   query.exec();

   if (!query.isActive())
      L_ERRO(QString("[rmHashes] SQL Error: %1").arg(query.lastError().text()));

   query.finish();
}

void HashCache::updateDatabaseScheme()
{
   QSqlQuery query(this->db);
   query.exec(
      R"(
SELECT [name] FROM [sqlite_master]
WHERE [type] = 'table' AND [name] = 'Version'
      )");

   int currentVersion = 0;

   if (query.first())
   {
      QSqlQuery queryVersion(this->db);
      queryVersion.exec(R"(SELECT [version] FROM [Version] ORDER BY [id] DESC)");
      if (queryVersion.first())
      {
         currentVersion = queryVersion.value(0).toInt();
      }
   }

   L_DEBU(QString("HashCache database version: %1").arg(currentVersion));

   try
   {
      forever
      {
         if (updateToNextVersion(currentVersion))
         {
            currentVersion += 1;
            L_DEBU(QString("HashCache database updated to version: %1").arg(currentVersion));
            QSqlQuery queryUpdateVersion(this->db);
            queryUpdateVersion.prepare("INSERT INTO [Version] ([version]) VALUES ($1)");
            queryUpdateVersion.bindValue(0, currentVersion);
            queryUpdateVersion.exec();
         }
         else
         {
            break;
         }
      }
   }
   catch (DatabaseException& e)
   {
      L_ERRO(QString("SQL error during update: %1").arg(e.error.text()));
   }
}

bool HashCache::updateToNextVersion(int currentVersion)
{
   switch (currentVersion)
   {
   case 0: // Version 0 to 1.
      {
         QSqlQuery queryInitialDatabase(this->db);
         this->db.transaction();
         for (const QString& statement : HashCache::VERSION_1)
         {
            queryInitialDatabase.exec(statement);
            if (!queryInitialDatabase.isActive())
            {
               this->db.rollback();
               throw DatabaseException(queryInitialDatabase.lastError());
            }
         }
         this->db.commit();

         return true;
      }
   }

   return false;
}

const QStringList HashCache::VERSION_1 =
{
   R"(
-- Version 1 is the initial structure.
CREATE TABLE [Version] (
   [id] INTEGER PRIMARY KEY,
   [version] INTEGER NOT NULL UNIQUE
) STRICT;
   )",
   R"(
CREATE TABLE [File] (
[id] INTEGER PRIMARY KEY,
[path] TEXT NOT NULL,
[size] INTEGER NOT NULL, -- [Byte]
[date_last_modified] INTEGER NOT NULL, -- [ms] Since epoch.
[hashes] BLOB not null -- Concatenated hashes, each hash is Common::Hash::HASH_SIZE long.
) STRICT;
   )",
   R"(
CREATE UNIQUE INDEX [File_path_index] ON [File]([path]);
   )"
};
