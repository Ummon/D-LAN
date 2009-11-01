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

SharedDirectory::SharedDirectory(Cache* cache, const QString& path, Rights rights, const Common::Hash& id)
   : Directory(), cache(cache), path(path), rights(rights), id(id)
{
   this->cache->onEntryAdded(this);
}

SharedDirectory::~SharedDirectory()
{
   this->cache->onEntryRemoved(this);
}

QList<File*> SharedDirectory::restoreFromFileCache(const Protos::FileCache::Hashes& hashes)
{
   QList<File*> ret;

   // Give each root to each sub directory. We don't match the full path.
   for (int i = 0; i < hashes.dir_size(); i++)
      ret << Directory::restoreFromFileCache(hashes.dir(i).root());

   return ret;
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

const Common::Hash& SharedDirectory::getId()
{
   return this->id;
}
