#pragma once

#include <Common/Hash.h>
#include <IEntry.h>

namespace HC
{
   class IFile : public IEntry
   {
   public:
      virtual ~IFile() {}

      /**
        * All hashes must be provided. Some hashes may be null (Comm::Hash::isNull).
        */
      virtual QList<Common::Hash> getHashes() = 0;
      virtual void setHashes(QList<Common::Hash> hashes) = 0;

      virtual qint64 getSize() = 0;
      virtual QDateTime getDateLastModified() = 0;
   };
}
