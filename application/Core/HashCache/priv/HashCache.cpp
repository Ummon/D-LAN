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

#include <optional>
#include <algorithm>

#include <QFile>
#include <QScopeGuard>
#include <QTimeZone>
#include <QSqlDatabase>
#include <QSqlQuery>
#include <QUuid>
#include <QSqlError>

#include <Common/Global.h>
#include <Common/Hash.h>
#include <Common/Constants.h>

#include <Common/Settings.h>
#include <priv/Log.h>
#include <priv/Exceptions.h>

class HashCache::Database
{
public:
   explicit Database(const QString& databaseFolder);
   ~Database();

   QList<Common::Hash> getHashes(const QString& filePath, qint64 size, QDateTime timeLastModified);
   void setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime);
   void rmHashes(const QString& filePath);
   int checkFilesExist(); // Returns the delay in milliseconds until the next check.

private:
   LOG_INIT_H("HashCache")

   template <typename T>
   std::optional<T> getSettings(const QString& key);

   template <typename T>
   void setSettings(const QString& key, T value);

   static const QString LAST_CHECK_TIME_KEY;
   static const QString NB_DELETED_FILES_KEY;

   QDateTime getLastCheckTime();
   void setLastCheckTime(QDateTime dateTime);

   quint64 getNbDeletedFiles();
   void setNbDeletedFiles(quint64 n);

   void updateDatabaseScheme();
   bool updateToNextVersion(int currentVersion);

   QSqlDatabase db;
   std::optional<QSqlQuery> queryGetHashesWithDate;
   std::optional<QSqlQuery> queryGetHashes;
   std::optional<QSqlQuery> querySetHashes;
   std::optional<QSqlQuery> queryRemoveHashes;
   std::optional<QSqlQuery> queryNbOfFiles;
   std::optional<QSqlQuery> queryAllFiles;
   std::optional<QSqlQuery> queryGetSettings;
   std::optional<QSqlQuery> querySetSettings;

   static const QStringList VERSION_1;
   static const QStringList VERSION_2;
   static const QStringList VERSION_3;
};

LOG_INIT_CPP(HashCache::Database)

HashCache::HashCache(const QString& databaseFolder, int initialFileCheckDelay) :
   databaseContext(new QObject)
{
   this->databaseThread.setObjectName("HashCache");
   this->databaseContext->moveToThread(&this->databaseThread);
   QObject::connect(&this->databaseThread, &QThread::finished, this->databaseContext, &QObject::deleteLater);
   this->databaseThread.start();

   QMetaObject::invokeMethod(
      this->databaseContext,
      [this, &databaseFolder, initialFileCheckDelay]
      {
         this->database = std::make_unique<Database>(databaseFolder);
         const quint32 period = SETTINGS.get<quint32>("hashcache_period_verify_files_exist");
         if (period > 0)
         {
            this->checkDeletedFileTimer = new QTimer(this->databaseContext);
            this->checkDeletedFileTimer->setTimerType(Qt::PreciseTimer);
            this->checkDeletedFileTimer->setSingleShot(true);
            const auto checkFiles = [this]
            {
               this->checkDeletedFileTimer->start(this->database->checkFilesExist());
            };
            this->checkDeletedFileTimer->callOnTimeout(this->databaseContext, checkFiles);
            if (initialFileCheckDelay == 0)
               QMetaObject::invokeMethod(this->databaseContext, checkFiles, Qt::QueuedConnection);
            else
               this->checkDeletedFileTimer->start(initialFileCheckDelay);
         }
      },
      Qt::BlockingQueuedConnection
   );
}

HashCache::~HashCache()
{
   QMetaObject::invokeMethod(
      this->databaseContext,
      [this]
      {
         delete this->checkDeletedFileTimer;
         this->database.reset();
      },
      Qt::BlockingQueuedConnection
   );

   this->databaseThread.quit();
   this->databaseThread.wait();
}

QList<Common::Hash> HashCache::getHashes(const QString& filePath, qint64 size, QDateTime timeLastModified)
{
   QList<Common::Hash> result;

   QMetaObject::invokeMethod(
      this->databaseContext,
      [this, &filePath, size, timeLastModified, &result]
      {
         result = this->database->getHashes(filePath, size, timeLastModified);
      },
      Qt::BlockingQueuedConnection
   );

   return result;
}

void HashCache::setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime)
{
   QMetaObject::invokeMethod(
      this->databaseContext,
      [this, filePath, hashes, size, dateTime]
      {
         this->database->setHashes(filePath, hashes, size, dateTime);
      },
      Qt::QueuedConnection
   );
}

void HashCache::rmHashes(const QString& filePath)
{
   QMetaObject::invokeMethod(
      this->databaseContext, [this, filePath]
      {
         this->database->rmHashes(filePath);
      },
      Qt::QueuedConnection
   );
}

/////

HashCache::Database::Database(const QString& databaseFolder) :
   db { QSqlDatabase::addDatabase("QSQLITE", QUuid::createUuid().toString(QUuid::WithoutBraces)) },
   queryGetHashesWithDate(this->db),
   queryGetHashes(this->db),
   querySetHashes(this->db),
   queryRemoveHashes(this->db),
   queryNbOfFiles(this->db),
   queryAllFiles(this->db),
   queryGetSettings(this->db),
   querySetSettings(this->db)
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

   this->queryGetHashesWithDate->prepare(
      "SELECT [hashes], [size] FROM [File] WHERE [path] = $1 AND [size] = $2 AND [date_last_modified] = $3"
   );

   this->queryGetHashes->prepare("SELECT [hashes], [size] FROM [File] WHERE [path] = $1 AND [size] = $2");

   this->querySetHashes->prepare(
      R"(
INSERT INTO [File] ([path], [size], [date_last_modified], [hashes])
VALUES ($1, $2, $3, $4)
ON CONFLICT([path]) DO
UPDATE SET [path] = $1, [size] = $2, [date_last_modified] = $3, [hashes] = $4
      )"
   );

   this->queryRemoveHashes->prepare("DELETE FROM [File] WHERE [path] = $1");

   this->queryNbOfFiles->prepare("SELECT COUNT(*) FROM [File]");

   this->queryAllFiles->prepare("SELECT [id], [path] FROM [File]");

   this->queryGetSettings->prepare(
      "SELECT [value] FROM [Settings] WHERE [key] = $1 LIMIT 1"
   );

   this->querySetSettings->prepare(
      "INSERT INTO [Settings] ([key], [value]) VALUES($1, $2) ON CONFLICT([key]) DO UPDATE SET value = excluded.value"
   );
}

HashCache::Database::~Database()
{
   const QString connectionName = this->db.connectionName();

   // Release every query and database handle before unregistering the connection.
   this->queryGetHashesWithDate.reset();
   this->queryGetHashes.reset();
   this->querySetHashes.reset();
   this->queryRemoveHashes.reset();
   this->queryNbOfFiles.reset();
   this->queryAllFiles.reset();
   this->queryGetSettings.reset();
   this->querySetSettings.reset();

   this->db.close();
   this->db = QSqlDatabase();
   QSqlDatabase::removeDatabase(connectionName);
   L_DEBU("HashCache deleted");
}

QList<Common::Hash> HashCache::Database::getHashes(const QString& filePath, qint64 size, QDateTime timeLastModified)
{
   L_DEBU(QString("[getHashes] filePath: %1").arg(filePath));

   QSqlQuery& query = timeLastModified.isNull() ? *this->queryGetHashes : *this->queryGetHashesWithDate;
   query.bindValue(0, filePath);
   query.bindValue(1, size);

   if (!timeLastModified.isNull())
      query.bindValue(2, timeLastModified.toMSecsSinceEpoch());

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
      const qint64 storedSize = query.value(1).toLongLong();
      const int nbHashes = Common::Global::nbChunks(storedSize);

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

void HashCache::Database::setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime)
{
   L_DEBU(QString("[setHashes] filePath: %1").arg(filePath));

   QByteArray hashesBlob;
   hashesBlob.reserve(hashes.size() * Common::Hash::HASH_SIZE);
   for (int i = 0; i < hashes.size(); ++i)
   {
      hashesBlob.append(hashes[i].getData(), Common::Hash::HASH_SIZE);
   }

   QSqlQuery& query = *this->querySetHashes;

   query.bindValue(0, filePath);
   query.bindValue(1, size);
   query.bindValue(2, dateTime.isNull() ? 0 : dateTime.toMSecsSinceEpoch());
   query.bindValue(3, hashesBlob);
   query.exec();

   if (!query.isActive())
      L_ERRO(QString("[setHashes] SQL Error: %1").arg(query.lastError().text()));

   query.finish();
}

void HashCache::Database::rmHashes(const QString& filePath)
{
   L_DEBU(QString("[rmHashes] filePath: %1").arg(filePath));

   try
   {
      if (!this->db.transaction())
         throw DatabaseException(this->db.lastError());
      auto rollback = qScopeGuard([this] { this->db.rollback(); });
      QSqlQuery& query = *this->queryRemoveHashes;
      const auto finish = qScopeGuard([&query] { query.finish(); });
      query.bindValue(0, filePath);
      if (!query.exec())
         throw DatabaseException(query.lastError());
      const qint64 deleted = query.numRowsAffected();
      query.finish();
      if (deleted > 0)
         this->setNbDeletedFiles(this->getNbDeletedFiles() + quint64(deleted));
      if (!this->db.commit())
         throw DatabaseException(this->db.lastError());
      rollback.dismiss();
   }
   catch (DatabaseException& e)
   {
      L_ERRO(QString("[rmHashes] SQL Error: %1").arg(e.error.text()));
   }
}

int HashCache::Database::checkFilesExist()
{
   const qint64 periodMs = qint64(SETTINGS.get<quint32>("hashcache_period_verify_files_exist")) * 1000;
   const quint32 minFiles = SETTINGS.get<quint32>("hashcache_nb_of_files_before_check");
   const quint32 minDeleted = SETTINGS.get<quint32>("hashcache_nb_of_files_deleted_before_vacuum");
   // Recheck long periods in daily steps to stay within QTimer's int range.
   const auto timerDelay = [](qint64 ms) { return int(std::clamp(ms, qint64(1), qint64(86400000))); };
   const QDateTime now = QDateTime::currentDateTimeUtc();

   try
   {
      const QDateTime lastCheck = this->getLastCheckTime();
      if (lastCheck.isValid() && lastCheck <= now && lastCheck.msecsTo(now) < periodMs)
         return timerDelay(periodMs - lastCheck.msecsTo(now));

      QList<qint64> idsToDelete;
      {
         QSqlQuery& count = *this->queryNbOfFiles;
         const auto finish = qScopeGuard([&count] { count.finish(); });
         if (!count.exec() || !count.first())
            throw DatabaseException(count.lastError());
         const quint64 nbFiles = count.value(0).toULongLong();
         count.finish();

         if (nbFiles > minFiles)
         {
            QSqlQuery& files = *this->queryAllFiles;
            const auto finishFiles = qScopeGuard([&files] { files.finish(); });
            if (!files.exec())
               throw DatabaseException(files.lastError());
            while (files.next())
            {
               if (!QFile::exists(files.value(1).toString()))
                  idsToDelete << files.value(0).toLongLong();
            }
            if (files.lastError().isValid())
               throw DatabaseException(files.lastError());
         }
      }

      quint64 filesDeletedTotal;
      {
         if (!this->db.transaction())
            throw DatabaseException(this->db.lastError());
         auto rollback = qScopeGuard([this] { this->db.rollback(); });
         filesDeletedTotal = this->getNbDeletedFiles();
         if (!idsToDelete.isEmpty())
         {
            // Finish the scan before modifying File. Reusing a single bound
            // parameter also avoids SQLite's limit on parameters in an IN list.
            QSqlQuery remove(this->db);
            if (!remove.prepare("DELETE FROM [File] WHERE [id] = ?"))
               throw DatabaseException(remove.lastError());
            for (qint64 id : idsToDelete)
            {
               remove.bindValue(0, id);
               if (!remove.exec())
                  throw DatabaseException(remove.lastError());
               filesDeletedTotal += quint64(remove.numRowsAffected());
            }
         }
         this->setNbDeletedFiles(filesDeletedTotal);
         this->setLastCheckTime(now);
         if (!this->db.commit())
            throw DatabaseException(this->db.lastError());
         rollback.dismiss();
      }

      // Vacuum outside the transaction, with all queries finished. Check even
      // below minFiles: previous deletions may already warrant compaction.
      if (filesDeletedTotal > minDeleted)
      {
         QSqlQuery vacuum(this->db);
         if (!vacuum.exec("VACUUM"))
            throw DatabaseException(vacuum.lastError());
         vacuum.finish();
         this->setNbDeletedFiles(0);

         // In WAL mode VACUUM can leave the compacted pages in the WAL.
         // Checkpoint them so disk space is reclaimed while the cache is open.
         QSqlQuery checkpoint(this->db);
         if (!checkpoint.exec("PRAGMA wal_checkpoint(TRUNCATE)") || !checkpoint.first())
            throw DatabaseException(checkpoint.lastError());
         if (checkpoint.value(0).toInt() != 0)
            L_WARN("[checkFilesExist] WAL checkpoint is busy; disk space reclamation is deferred");
      }
      return timerDelay(periodMs - now.msecsTo(QDateTime::currentDateTimeUtc()));
   }
   catch (DatabaseException& e)
   {
      L_ERRO(QString("[checkFilesExist] SQL Error: %1").arg(e.error.text()));
   }
   return timerDelay(periodMs);
}

template <typename T>
std::optional<T> HashCache::Database::getSettings(const QString& key)
{
   QSqlQuery& query = *this->queryGetSettings;
   const auto finish = qScopeGuard([&query] { query.finish(); });
   query.bindValue(0, key);
   if (!query.exec())
      throw DatabaseException(query.lastError());

   if (query.first())
   {
      QVariant value = query.value(0);
      if (!value.isNull() && value.convert(QMetaType::fromType<T>()))
         return value.value<T>();
   }
   if (query.lastError().isValid())
      throw DatabaseException(query.lastError());

   return std::nullopt;
}

template <typename T>
void HashCache::Database::setSettings(const QString& key, T value)
{
   QSqlQuery& query = *this->querySetSettings;
   const auto finish = qScopeGuard([&query] { query.finish(); });

   query.bindValue(0, key);
   query.bindValue(1, QVariant::fromValue(value));
   if (!query.exec())
      throw DatabaseException(query.lastError());
}

const QString HashCache::Database::LAST_CHECK_TIME_KEY("last_check_time");
const QString HashCache::Database::NB_DELETED_FILES_KEY("nb_deleted_files");

QDateTime HashCache::Database::getLastCheckTime()
{
   const auto timestamp = this->getSettings<qint64>(LAST_CHECK_TIME_KEY);
   return timestamp ? QDateTime::fromMSecsSinceEpoch(*timestamp, QTimeZone::UTC) : QDateTime();
}

void HashCache::Database::setLastCheckTime(QDateTime dateTime)
{
   this->setSettings(LAST_CHECK_TIME_KEY, dateTime.toMSecsSinceEpoch());
}

quint64 HashCache::Database::getNbDeletedFiles()
{
   return this->getSettings<quint64>(NB_DELETED_FILES_KEY).value_or(0);
}

void HashCache::Database::setNbDeletedFiles(quint64 n)
{
   this->setSettings(NB_DELETED_FILES_KEY, n);
}

void HashCache::Database::updateDatabaseScheme()
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

   query.finish(); // Release the sqlite_master cursor before schema changes.
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
bool HashCache::Database::updateToNextVersion(int currentVersion)
{
   const QStringList* statements = nullptr;

   switch (currentVersion)
   {
   case 0: // Version 0 to 1.
      statements = &HashCache::Database::VERSION_1;
      break;

   case 1: // Version 1 to 2.
      statements = &HashCache::Database::VERSION_2;
      break;

   case 2: // Version 2 to 3: permit typed settings in existing databases.
      statements = &HashCache::Database::VERSION_3;
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

const QStringList HashCache::Database::VERSION_1 =
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

const QStringList HashCache::Database::VERSION_2 =
{
   R"(
-- Version 2 add the Settings table.
CREATE TABLE [Settings] (
   [key] TEXT PRIMARY KEY NOT NULL,
   [value] BLOB
) STRICT;
   )",
};

const QStringList HashCache::Database::VERSION_3 =
{
   "CREATE TABLE [Settings_new] ([key] TEXT PRIMARY KEY NOT NULL, [value] ANY) STRICT",
   "INSERT INTO [Settings_new] SELECT [key], [value] FROM [Settings]",
   "DROP TABLE [Settings]",
   "ALTER TABLE [Settings_new] RENAME TO [Settings]"
};
