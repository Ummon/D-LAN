#pragma once

#include <Core/HashCache/IHashCache.h>

class MockHashCache :  public HC::IHashCache
{
public:
   MockHashCache();

   Common::Hashes getHashes(const QString& filePath) override;
   void setHashes(QString& filePath, const QList<QString>& filePaths) override;
   void setSizeAndDateTime(QString& filePath, qint64 size, QDateTime dateTime) override;
   void rmHashes(QString& filePath) override;
};
