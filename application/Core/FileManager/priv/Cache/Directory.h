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

#include <functional>

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
      friend class Entry;

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
      void populateContent(Protos::Common::Entries* entries, bool setSharedDirs, int maxNbHashesPerFile) const;

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

      bool isAChildOf(const Directory* dir) const;

      Directory* getSubDir(const QString& name) const;
      QList<Directory*> getSubDirs() const;

      QList<File*> getFiles() const;
      QList<File*> getCompleteFiles() const;
      bool isEmpty() const;

      Directory* createSubDir(const QString& name, bool physically = false, bool isHidden = false);
      Directory* createSubDirs(const QStringList& names, bool physically = false);

      File* getFile(const QString& name) const;
      void add(File* file);
      void fileSizeChanged(qint64 oldSize, qint64 newSize);

      void stealContent(Directory* dir);
      void add(Directory* dir);

      bool isScanned() const;
      void setScanned(bool value);

   protected:
      void setRootRecursively(SharedEntry* sharedEntry) override;

   private:
      void updateEntryName(Entry* entry, const std::function<void()>& update);

      void adjustSize(qint64 delta);

      /**
        * The sort key of the entries: their name compared without its case.
        * It shares the string of the entry: a lower case copy would have to be allocated for each comparison, the
        * main cost of a lookup, or be kept in each entry, about a hundred bytes more for each of them.
        */
      struct NameKey
      {
         QString name;
         friend bool operator<(const NameKey& k1, const NameKey& k2) { return k1.name.compare(k2.name, Qt::CaseInsensitive) < 0; }
         friend bool operator==(const NameKey& k1, const NameKey& k2) { return k1.name.compare(k2.name, Qt::CaseInsensitive) == 0; }
      };
      static inline NameKey entryGetKeyFun(const Entry* const& entry) { return { entry->getName() }; }

      Common::SortedList<Directory*, NameKey> subDirs; ///< Sorted by name, without its case.
      Common::SortedList<File*, NameKey> files; ///< Sorted by name, without its case.

      bool scanned;
      QRecursiveMutex retirementMutex; ///< Serializes subtree retirement without blocking metadata callbacks.
      bool deletingChildren = false; ///< Guards reentrant/concurrent del() while children are being retired.
   };
}
