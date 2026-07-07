#pragma once

#include <Core/HashCache/IHashCache.h>

class MockHashCache :  public HC::IHashCache
{
public:
   MockHashCache();

   virtual QList<Common::Hash> getHashes(
      const QString& filePath,
      QDateTime timeLastModified = QDateTime()
   ) override;

   /**
     * Set all hashes for the given file path.
     * If the number of hased doesn't match the file size, the request is rejected.
     */
   virtual void setHashes(
      const QString& filePath,
      const QList<Common::Hash>& hashes,
      qint64 size,
      QDateTime dateTime = QDateTime()
   ) override;

   virtual void rmHashes(const QString& filePath) override;
};
