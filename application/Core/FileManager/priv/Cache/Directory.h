/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
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
