#ifndef HASHCACHE_HASHCACHE_H
#define HASHCACHE_HASHCACHE_H

#include <QHash>
#include <QString>

#include <Common/Hash.h>

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

      QHash<QString, Common::Hash> index; // Directory paths -> root ID.


      LOG_INIT_H("HashCache")
   };
}

#endif
