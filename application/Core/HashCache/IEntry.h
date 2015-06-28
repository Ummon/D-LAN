#ifndef HASHCACHE_IENTRY_H
#define HASHCACHE_IENTRY_H

#include <QString>

namespace HC
{
   class IEntry
   {
   public:
      virtual ~IEntry() {}

      virtual QString getName() = 0;
   };
}

#endif
