#include <priv/HashCache.h>
using namespace HC;

#include <QDir>
#include <QSqlQuery>
#include <QSqlError>

#include <Common/Global.h>
#include <Common/Hash.h>
#include <Common/Constants.h>

#include <priv/Log.h>

LOG_INIT_CPP(HashCache)

HashCache::HashCache(const QString& databaseFolder) :
   db { QSqlDatabase::addDatabase("QSQLITE") }
{
   this->db.setDatabaseName(QString("%1/%2").arg(databaseFolder, Common::Constants::HASH_CACHE_INDEX_FILENAME));

   if (!db.open()) {
      L_ERRO(QString("Unable to open hash cache index database: %1").arg(db.lastError().text()));
   }
}

QList<Common::Hash> HashCache::getHashes(const QString& filePath)
{
   QSqlQuery query(this->db);
   query.exec("SELECT * FROM...");

   return QList<Common::Hash>();
}

void HashCache::setHashes(QString& filePath, const QList<QString>& filePaths)
{

}

void HashCache::setSizeAndDateTime(QString& filePath, qint64 size, QDateTime dateTime)
{

}

void HashCache::rmHashes(QString& filePath)
{

}
