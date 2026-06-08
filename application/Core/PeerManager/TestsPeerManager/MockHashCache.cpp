#include <MockHashCache.h>

#include <QDateTime>

MockHashCache::MockHashCache() {}

QList<Common::Hash> MockHashCache::getHashes(const QString& filePath)
{
   return QList<Common::Hash>();
}

void MockHashCache::setHashes(QString& filePath, const QList<QString>& filePaths)
{
}

void MockHashCache::setSizeAndDateTime(QString& filePath, qint64 size, QDateTime dateTime)
{
}

void MockHashCache::rmHashes(QString& filePath)
{
}