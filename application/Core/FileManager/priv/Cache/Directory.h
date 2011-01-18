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
      void changeName(const QString& newName);
      bool isAChildOf(const Directory* dir) const;

      Directory* getSubDir(const QString& name) const;
      QList<Directory*> getSubDirs() const;
      QList<File*> getFiles() const;
      QList<File*> getCompleteFiles() const;

      Directory* createSubDirectory(const QString& name, bool physically = false);
      Directory* createSubDirectories(const QStringList& names, bool physically = false);

      File* getFile(const QString& name) const;      
      void add(File* file);
      void fileSizeChanged(qint64 oldSize, qint64 newSize);

      void stealContent(Directory* dir);
      void add(Directory* dir);

   private:
      void subdirNameChanged(Directory* dir);

   public:
      void fileNameChanged(File* file);

   private:
      void add(QList<Directory*> dirs);
      void add(QList<File*> files);

      template <typename T>
      static void sortedAdd(T* entry, QList<T*>& list);
      template <typename T>
      static void sortedAdd(const QList<T*>& entries, QList<T*>& list);

      Directory& operator+=(qint64);
      Directory& operator-=(qint64);

      Directory* parent;

      QList<Directory*> subDirs; ///< Sorted by name.
      QList<File*> files; ///< Sorted by name.

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

using namespace FM;

template <typename T>
void Directory::sortedAdd(T* entry, QList<T*>& list)
{
   for (QMutableListIterator<T*> i(list); i.hasNext(); i.next())
   {
      T* e = i.peekNext();
      if (e == entry)
         return;
      if (entry->getName() < e->getName())
      {
         i.insert(entry);
         return;
      }
   }

   list << entry;
}

template <typename T>
void Directory::sortedAdd(const QList<T*>& entries, QList<T*>& list)
{
   QListIterator<T*> i(entries);
   QMutableListIterator<T*> j(list);

   while(i.hasNext())
   {
      T* ei = i.next();

      bool inserted = false;
      while (j.hasNext())
      {
         T* ej = j.peekNext();
         if (ej->getName() > ei->getName())
         {
            j.insert(ei);
            i.next();
            inserted = true;
            break;
         }
         j.next();
      }

      if (!inserted)
         j.insert(ei);
   }
}

#endif
