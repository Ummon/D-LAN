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

#pragma once

#include <QString>
#include <QList>
#include <QFileInfo>
#include <QMutex>
#include <QMap>

#include <Protos/common.pb.h>

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

   public:
      Directory(
         SharedEntry* root,
         const QString& name,
         Directory* parentDirectory = nullptr,
         bool createPhysically = false,
         bool hidden = false
      );

      ~Directory() override;
      void del(bool invokeDelete = true) override;

      void populateEntry(Protos::Common::Entry* dir, bool setSharedDir = false) const override;

      void removeUnfinishedFiles() override;

      void moveInto(Directory* directory) override;

      void fileDeleted(File* file);

   private:
      void subDirDeleted(Directory* dir);

   public:
      /**
        * The top directory will return '/' because it carries the shared directory name.
        */
      Common::Path getRelativePath() const override;
      Common::Path getAbsolutePath() const override;
      Entry* getEntry(const Common::Path& path) override;

      void rename(const QString& newName) override;
      bool isAChildOf(const Directory* dir) const;

      Directory* getSubDir(const QString& name) const;
      QList<Directory*> getSubDirs() const;

      QList<File*> getFiles() const;
      QList<File*> getCompleteFiles() const;

      Directory* createSubDir(const QString& name, bool physically = false, bool isHidden = false);
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
      void setRootRecursively(SharedEntry* sharedEntry) override;

   private:
      void subdirNameChanged(Directory* dir);

      Directory& operator+=(qint64);
      Directory& operator-=(qint64);

      static inline bool entrySortingFun(const Entry* const& e1, const Entry* const& e2) { return (*e1) < (*e2); }

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
      QList<Directory*> dirsToVisit;
   };
}
