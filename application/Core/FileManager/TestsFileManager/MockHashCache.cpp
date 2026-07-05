#include <MockHashCache.h>

#include <QDateTime>

MockHashCache::MockHashCache() {}

QList<Common::Hash> MockHashCache::getHashes(const QString& filePath, QDateTime timeLastModified)
{
   return QList<Common::Hash>();
}

void MockHashCache::setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime)
{
}

void MockHashCache::rmHashes(const QString& filePath)
{
}