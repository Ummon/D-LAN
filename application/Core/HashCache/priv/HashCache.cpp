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
   const QString DATABASE_FILEPATH = QString("%1/%2").arg(databaseFolder, Common::Constants::HASH_CACHE_INDEX_FILENAME);
   L_DEBU(QString("HashCache database: %1").arg(DATABASE_FILEPATH));

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
         if (!this->db.transaction())
            throw DatabaseException(this->db.lastError());

         try
         {
            if (!this->updateToNextVersion(currentVersion))
            {
               this->db.rollback();
               break;
            }

            // The version row is written in the same transaction as the migration itself,
            // otherwise a crash between the two would leave the schema updated but the version not.
            QSqlQuery queryUpdateVersion(this->db);
            queryUpdateVersion.prepare("INSERT INTO [Version] ([version]) VALUES (?)");
            queryUpdateVersion.bindValue(0, currentVersion + 1);
            if (!queryUpdateVersion.exec())
               throw DatabaseException(queryUpdateVersion.lastError());

            if (!this->db.commit())
               throw DatabaseException(this->db.lastError());
         }
         catch (DatabaseException&)
         {
            this->db.rollback();
            throw;
         }

         currentVersion += 1;
         L_DEBU(QString("HashCache database updated to version: %1").arg(currentVersion));
      }
   }
   catch (DatabaseException& e)
   {
      L_ERRO(QString("SQL error during update: %1").arg(e.error.text()));
   }
}

/**
  * Applies the migration from 'currentVersion' to 'currentVersion + 1'.
  * Must be called inside a transaction, the caller is responsible to commit or rollback.
  * Returns false if there is no migration from the given version (the database is up to date).
  * @exception DatabaseException
  */
bool HashCache::updateToNextVersion(int currentVersion)
{
   const QStringList* statements = nullptr;

   switch (currentVersion)
   {
   case 0: // Version 0 to 1.
      statements = &HashCache::VERSION_1;
      break;

   default:
      return false;
   }

   QSqlQuery query(this->db);
   for (const QString& statement : *statements)
   {
      if (!query.exec(statement))
         throw DatabaseException(query.lastError());
   }

   return true;
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
