#include <MockHashCache.h>

#include <QDateTime>

MockHashCache::MockHashCache() {}

QList<Common::Hash> MockHashCache::getHashes(
   const QString& filePath,
   QDateTime timeLastModified
)
{
   return QList<Common::Hash>();
}

/**
  * Set all hashes for the given file path.
  * If the number of hashes doesn't match the file size, the request is rejected.
  */
void MockHashCache::setHashes(
   const QString& filePath,
   const QList<Common::Hash>& hashes,
   qint64 size,
   QDateTime dateTime
)
{
}


void MockHashCache::rmHashes(const QString& filePath)
{
}