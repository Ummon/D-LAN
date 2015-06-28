#include <priv/HashCache.h>
using namespace HC;

#include <QDir>

#include <Common/Global.h>
#include <Common/Constants.h>

LOG_INIT_CPP(HashCache)

HashCache::HashCache()
{
   if (!QDir(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL)).exists(Common::Constants::HASH_CACHE_DIR))
      QDir(Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL)).mkdir(Common::Constants::HASH_CACHE_DIR);
}

void HashCache::loadTo(IRootEntry& root)
{

}

void HashCache::saveFrom(const IRootEntry& root)
{

}
