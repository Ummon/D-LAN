#pragma once

#include <Core/HashCache/IHashCache.h>

class MockHashCache :  public HC::IHashCache
{
public:
   MockHashCache();

   QList<Common::Hash> getHashes(const QString& filePath, QDateTime timeLastModified = QDateTime()) override;
   void setHashes(const QString& filePath, const QList<Common::Hash>& hashes, qint64 size, QDateTime dateTime = QDateTime()) override;
   void rmHashes(const QString& filePath) override;
};
