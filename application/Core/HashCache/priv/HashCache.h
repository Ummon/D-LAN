#pragma once

#include <QString>

#include <Common/Hash.h>
#include <Common/Path.h>

#include <IHashCache.h>
#include <IRootEntry.h>
#include <priv/Log.h>

namespace HC
{
   class HashCache : public IHashCache
   {
   public:
      HashCache();

      void loadTo(IRootEntry& root);
      void saveFrom(const IRootEntry& root);

   private:
      void updateFromOldVersion();

      struct Entry
      {
         Common::Path path;
         Common::Hash ID;
      };

      QList<Entry> index; // Directory paths -> root ID.


      LOG_INIT_H("HashCache")
   };
}
