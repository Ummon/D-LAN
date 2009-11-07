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

      /**
        * Create from a saved shared directory (file cache).
        * If a existing shared directory is a sub directory then it will be merged.
        * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
        * @exception SubDirectoriesWithDifferentRightsExistsException Thrown when one or more sub directory already exists with different rights.
        */
      SharedDirectory(Cache* cache, const QString& path, Rights rights, const Common::Hash& id);
      SharedDirectory(Cache* cache, const QString& path, Rights rights);

   private:
      void init();

   public:
      ~SharedDirectory();

      QList<File*> restoreFromFileCache(const Protos::FileCache::Hashes& hashes);

      /**
        * Return always "" thus it makes this method the most usefull of the entire known univers.
        */
      QString getPath() const;

      /**
        * Return the full path to the shared directory.
        * There is no slash at the end, only for the root.
        * For exemple :
        *  - '/home/paul/movies'
        *  - '/'.
        *  - 'C:/Users/Paul/My Movies'
        *  - 'G:/'
        */
      QString getFullPath() const;

      Rights getRights() const;

      const Common::Hash& getId() const;

   private:

      QString path;
      Rights rights;
      Common::Hash id;
   };
}
#endif
