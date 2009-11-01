#ifndef FILEMANAGER_SHAREDDIRECTORY_H
#define FILEMANAGER_SHAREDDIRECTORY_H

#include <QString>

#include <Common/Hash.h>
#include <priv/Cache/Directory.h>

namespace FM
{
   class Cache;
   class FileManager;

   class SharedDirectory : public Directory
   {
   public:
      enum Rights {
         READ_ONLY,
         READ_WRITE
      };

      SharedDirectory(Cache* cache, const QString& path, Rights rights);

      /**
        * Create from a saved shared directory (file cache).
        */
      SharedDirectory(Cache* cache, const QString& path, Rights rights, const Common::Hash& id);

      /**
        * Return always "" thus it makes this method the most usefull of the entire known univers.
        */
      QString getPath();

      /**
        * Return the full path to the shared directory.
        * For exemple : '/home/paul/movies'. (no slash at the end).
        */
      QString getFullPath();

      Cache* getCache();

      Rights getRights();

   private:
      Cache* cache;

      QString path;
      Rights rights;
      Common::Hash id;
   };

}
#endif
