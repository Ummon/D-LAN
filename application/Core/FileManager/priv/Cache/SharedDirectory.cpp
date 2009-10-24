#include <priv/Cache/SharedDirectory.h>
using namespace FM;

#include <priv/Cache/Cache.h>

SharedDirectory::SharedDirectory(Cache* cache, const QString& path, Rights rights)
   : Directory(), cache(cache), path(path), rights(rights)
{
   this->id = Common::Hash::rand();

   // Same as a new file (see the File ctor).
   this->cache->onEntryAdded(this);
}

QString SharedDirectory::getPath()
{
   return "";
}

QString SharedDirectory::getFullPath()
{
   return this->path;
}

Cache* SharedDirectory::getCache()
{
   return this->cache;
}

SharedDirectory::Rights SharedDirectory::getRights()
{
   return this->rights;
}
