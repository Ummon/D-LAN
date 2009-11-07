#ifndef FILEMANAGER_DIRECTORY_H
#define FILEMANAGER_DIRECTORY_H

#include <QString>
#include <QList>

#include <Protos/common.pb.h>
#include <Protos/files_cache.pb.h>
#include <priv/Cache/Entry.h>

namespace FM
{
   class File;
   class Cache;

   class Directory : public Entry
   {
   public:
      Directory(Directory* parent, const QString& name);

   protected:
      /**
        * Called by the root which will not have parent and name.
        */
      Directory(Cache* cache);

   public:

      virtual ~Directory();

      QList<File*> restoreFromFileCache(const Protos::FileCache::Hashes_Dir& dir);

      void populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill) const;

      void populateDirEntry(Protos::Common::DirEntry* entry) const;

      /**
        * Ask to delete all chunks is this directory.
        */
      void deleteChunks();

      /**
        * Called by one of its file destructor.
        */
      void fileDeleted(File* file);

   private:
      void subDirDeleted(Directory* dir);
      bool tryToSuicide();

   public:

      virtual QString getPath() const;
      virtual QString getFullPath() const;

      Directory* getRoot() const;

      /**
        * Only called by the class File.
        */
      void addFile(File* file);

      /**
        * Steal the sub directories from 'dir'.
        * The sub dirs will be removed from 'dir'.
        */
      void stealSubDirs(Directory* dir);

      /**
        * When a new file is added to a directory this method is called
        * to add its size.
        */
      void addSize(qint64);

      Directory* parent;

      QList<Directory*> subDirs;
      QList<File*> files;
   };
}
#endif
