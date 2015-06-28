#ifndef HASHCACHE_HASHCACHE_H
#define HASHCACHE_HASHCACHE_H

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

      LOG_INIT_H("HashCache")
   };
}

#endif
