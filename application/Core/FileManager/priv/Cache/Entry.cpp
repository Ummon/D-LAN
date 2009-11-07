#include "Entry.h"
using namespace FM;

#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/SharedDirectory.h>

Entry::Entry(Cache* cache, const QString& name, qint64 size)
   : cache(cache), name(name), size(size)
{
   this->cache->onEntryAdded(this);
}

Entry::~Entry()
{
   this->cache->onEntryRemoved(this);
}

Cache* Entry::getCache()
{
   return this->cache;
}

const QString& Entry::getName() const
{
   return this->name;
}

qint64 Entry::getSize() const
{
   return this->size;
}
