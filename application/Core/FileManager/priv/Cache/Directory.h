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

   class Directory : public Entry
   {
   public:
      Directory(Directory* parent, const QString& name);

      virtual ~Directory();

      QList<File*> restoreFromFileCache(const Protos::FileCache::Hashes_Dir& dir);

      void populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill);

      void populateDirEntry(Protos::Common::DirEntry* entry);

      virtual QString getPath();
      virtual QString getFullPath();

      Directory* getRoot();

      /**
        * Only called by the class File.
        */
      void addFile(File* file);

      /**
        * Steal the sub directories from 'dir'.
        * The sub dirs will be removed from 'dir'.
        */
      void stealSubDirs(Directory* dir);

   protected:
      Directory();

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
