#ifndef FILEMANAGER_DIRECTORY_H
#define FILEMANAGER_DIRECTORY_H

#include <QString>
#include <QList>
#include <QFileInfo>

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
      Directory(Directory* parent, const QString& name, bool createPhysically = false);

   protected:
      /**
        * Called by the root which will not have parent and name.
        */
      Directory(Cache* cache);

   public:

      virtual ~Directory();

      /**
        * Retore the hashes from the cache.
        * All file which are not complete and not in the cache are physically removed.
        * Only files ending with UNFINISHED_SUFFIX_TERM will be removed.
        * @return The files which have all theirs hashes (complete).
        */
      QList<File*> restoreFromFileCache(const Protos::FileCache::Hashes_Dir& dir);

      void populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill) const;

      void populateDirEntry(Protos::Common::DirEntry* entry) const;

      /**
        * Ask to delete all chunks is this directory.
        */
      //virtual void eliminate();

      /**
        * Called from one of its file.
        */
      void fileDeleted(File* file);

   private:
      void subDirDeleted(Directory* dir);

      //bool tryToSuicide();

   public:

      virtual QString getPath() const;
      virtual QString getFullPath() const;

      Directory* getRoot() const;

      /**
        * @return Returns 0 if no one match.
        */
      Directory* getSubDir(const QString& name) const;
      QList<Directory*> getSubDirs() const;
      QList<File*> getFiles() const;

      /**
        * Creates a new sub-directory if none exists already otherwise
        * returns an already existing.
        */
      Directory* createSubDirectory(const QString& name);

      /**
        * Creates a new sub-directory if none exists already otherwise
        * returns an already existing.
        */
      Directory* physicallyCreateSubDirectory(const QString& name);

      /**
        * Creates a new file if none exists already otherwise
        * checks if the size and the modification date match, if not then delete the
        * file and create a new one.
        */
      File* createFile(const QFileInfo& fileInfo);

      /**
        * Only called by the class File.
        */
      void addFile(File* file);

      /**
        * Steal the sub directories and files from 'dir'.
        * The sub dirs and files will be removed from 'dir'.
        */
      void stealContent(Directory* dir);

      /**
        * When a new file is added to a directory this method is called
        * to add its size.
        */
      Directory& operator+=(qint64);
      Directory& operator-=(qint64);

      Directory* parent;

      QList<Directory*> subDirs;
      QList<File*> files;
   };
}
#endif
