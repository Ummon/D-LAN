#ifndef FILEMANAGER_DIRECTORY_H
#define FILEMANAGER_DIRECTORY_H

#include <QString>
#include <QList>
#include <QFileInfo>
#include <QMutex>

#include <Protos/common.pb.h>
#include <Protos/files_cache.pb.h>
#include <priv/Cache/Entry.h>

namespace FM
{
   class File;
   class Cache;

   class Directory : public Entry
   {
      friend class DirIterator;

   public:
      Directory(Directory* parent, const QString& name, bool createPhysically = false);

   protected:
      Directory(Cache* cache, const QString& name);

   public:
      virtual ~Directory();

      QList<File*> restoreFromFileCache(const Protos::FileCache::Hashes_Dir& dir);

      void populateHashesDir(Protos::FileCache::Hashes_Dir& dirToFill) const;

      virtual void populateEntry(Protos::Common::Entry* dir, bool setSharedDir = false) const;

      void removeIncompleteFiles();

      void fileDeleted(File* file);

   private:
      void subDirDeleted(Directory* dir);

   public:
      virtual QString getPath() const;
      virtual QString getFullPath() const;

      Directory* getRoot() const;
      bool isAChildOf(const Directory* dir) const;

      Directory* getSubDir(const QString& name) const;
      QList<Directory*> getSubDirs() const;
      QList<File*> getFiles() const;
      QList<File*> getCompleteFiles() const;

      Directory* createSubDirectory(const QString& name);

      Directory* physicallyCreateSubDirectory(const QString& name);

//      File* createFile(const QFileInfo& fileInfo, File** oldFile);

      File* getFile(const QString& name) const;      
      void addFile(File* file);
      void fileSizeChanged(qint64 oldSize, qint64 newSize);

      void stealContent(Directory* dir);
      void append(Directory* dir);

   private:
      Directory& operator+=(qint64);
      Directory& operator-=(qint64);

      Directory* parent;

      QList<Directory*> subDirs;
      QList<File*> files;

      mutable QMutex mutex;
   };

   class DirIterator
   {
   public:
      DirIterator(Directory* dir);
      virtual ~DirIterator() {}
      Directory* next();

   private:
      QList<Directory*> dirsToVisit;
   };
}
#endif
