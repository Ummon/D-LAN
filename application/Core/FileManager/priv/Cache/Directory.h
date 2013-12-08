/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
#include <QMap>

#include <Protos/common.pb.h>
#include <Protos/files_cache.pb.h>

#include <Common/Containers/SortedList.h>

#include <priv/Cache/Entry.h>

namespace FM
{
   class File;
   class Cache;
   class SharedDirectory;

   class Directory : public Entry
   {
      friend class DirIterator;

   protected:
      Directory(Directory* parent, const QString& name, bool createPhysically = false);
      Directory(Cache* cache, const QString& name);

   public:
      virtual ~Directory();
      virtual void del(bool invokeDelete = true);

      QList<File*> restoreFromFileCache(const Protos::FileCache::Hashes::Dir& dir);
      void populateHashesDir(Protos::FileCache::Hashes::Dir& dirToFill) const;

      virtual void populateEntry(Protos::Common::Entry* dir, bool setSharedDir = false) const;

      virtual void removeUnfinishedFiles();

      virtual void moveInto(Directory* directory);

      void fileDeleted(File* file);

   private:
      void subDirDeleted(Directory* dir);

   public:
      virtual QString getPath() const;
      virtual QString getFullPath() const;
      virtual SharedDirectory* getRoot() const;

      void rename(const QString& newName);
      bool isAChildOf(const Directory* dir) const;

      Directory* getSubDir(const QString& name) const;
      QLinkedList<Directory*> getSubDirs() const;

      QLinkedList<File*> getFiles() const;
      QList<File*> getCompleteFiles() const;

      Directory* createSubDir(const QString& name, bool physically = false);
      Directory* createSubDirs(const QStringList& names, bool physically = false);

      File* getFile(const QString& name) const;      
      void add(File* file);
      void fileSizeChanged(qint64 oldSize, qint64 newSize);

      void stealContent(Directory* dir);
      void add(Directory* dir);

      bool isScanned() const;
      void setScanned(bool value);

      void fileNameChanged(File* file);

   protected:
      void deleteSubDirs();

   private:
      void subdirNameChanged(Directory* dir);

      Directory& operator+=(qint64);
      Directory& operator-=(qint64);

      static inline bool entrySortingFun(const Entry* const& e1, const Entry* const& e2) { return (*e1) < (*e2); }

      Directory* parent;

      Common::SortedList<Directory*> subDirs; ///< Sorted by name.
      Common::SortedList<File*> files; ///< Sorted by name.

      bool scanned;
   };

   class DirIterator
   {
   public:
      DirIterator(Directory* dir, bool includeRoot = false);
      virtual ~DirIterator() {}
      Directory* next();

   private:
      QLinkedList<Directory*> dirsToVisit;
   };
}

#endif
