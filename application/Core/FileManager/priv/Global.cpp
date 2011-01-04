#include <priv/Global.h>
using namespace FM;

#include <Common/Settings.h>

const QString& Global::getUnfinishedSuffix()
{
   static const QString suffix = SETTINGS.get<QString>("unfinished_suffix_term");
   return suffix;
}

bool Global::isFileUnfinished(const QString& filename)
{
   return filename.size() > Global::getUnfinishedSuffix().size() && filename.endsWith(Global::getUnfinishedSuffix());
}

QString Global::removeUnfinishedSuffix(const QString& filename)
{
   // Very special case : if this is an unfinished file and we didn't find the corresponding file in the cache (see 'getAllChunks' above'), we transform the local entry to a remote one.
   if (Global::isFileUnfinished(filename))
      return filename.left(filename.size() - Global::getUnfinishedSuffix().size());
   return filename;
}
