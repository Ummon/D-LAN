#include <MockHashCache.h>

#include <QDateTime>

MockHashCache::MockHashCache() {}

Common::Hashes MockHashCache::getHashes(const QString& filePath)
{
   return Common::Hashes();
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