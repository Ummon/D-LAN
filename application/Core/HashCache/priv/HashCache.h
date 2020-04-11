#pragma once

#include <QString>

#include <Common/Hash.h>
#include <Common/Path.h>

#include <IHashCache.h>
#include <priv/Log.h>

namespace HC
{
   class HashCache : public IHashCache
   {
   public:
      HashCache();

   private:


      LOG_INIT_H("HashCache")
   };
}
