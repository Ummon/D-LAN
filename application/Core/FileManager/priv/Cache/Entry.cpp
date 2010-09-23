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

void Entry::populateEntry(Protos::Common::Entry* entry) const
{
   entry->set_path(this->getPath().toStdString());
   entry->set_name(this->getName().toStdString());
   entry->set_size(this->getSize());
}

Cache* Entry::getCache()
{
   return this->cache;
}

const QString& Entry::getName() const
{
   return this->name;
}

void Entry::changeName(const QString& newName)
{
   this->name = newName;
}

qint64 Entry::getSize() const
{
   return this->size;
}
