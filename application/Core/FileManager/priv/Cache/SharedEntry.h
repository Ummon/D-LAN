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

#include <QStringList>
#include <QList>

#include <Protos/common.pb.h>

#include <Common/Uncopyable.h>
#include <Common/Hash.h>
#include <Common/Path.h>

#include <priv/Cache/Entry.h>

namespace FM
{
   class Cache;
   class FileManager;
   class Directory;
   class File;

   class SharedEntry : Common::Uncopyable
   {
      friend Directory;
      friend File;

   protected:
      SharedEntry(
         Cache* cache,
         const Common::Path& fullPath,
         const Common::Hash& id = Common::Hash(),
         const QString& userName = QString()
      );

   public:
      virtual ~SharedEntry();

      static SharedEntry* create(
         Cache* cache,
         const QString& pathStr,
         const Common::Hash& id = Common::Hash(),
         const QString& userName = QString()
      );

      static SharedEntry* create(
         Cache* cache,
         const Common::Path& path,
         const Common::Hash& id = Common::Hash(),
         const QString& userName = QString()
      );

      /**
        * Try to merge other shared entry with this one.
        * For exemple "/sharing/folder1/" can be merged with "/sharing/".
        * Should be called after each new SharedDirectory created.
        */
      virtual void mergeSubSharedEntries() = 0;
      // virtual Directory* createSubDirs(const QStringList& names, bool physically = false) = 0;
      virtual Entry* getRootEntry() const = 0;

      /**
        * Return the full path to the shared entry.
        * If the entry is a directory a slash is added at the end.
        * For exemple :
        *  - '/home/paul/movies/'
        *  - '/home/paul/movies/labyrinth.avi'
        *  - '/'.
        *  - 'C:/Users/Paul/My Movies/'
        *  - 'C:/Users/Paul/My Movies/labyrinth.avi'
        *  - 'G:/'
        */
      virtual Common::Path getPath() const = 0;

      void populateEntry(Protos::Common::Entry* entry) const;

      void del(bool invokeDelete = true);

      void moveInto(Directory* directory);

      // TODO: Common::Path should be used instead of QString.
      void setPath(const Common::Path& path);

      Cache* getCache() const;
      // Common::Path getPath() const;
      Common::Hash getId() const;
      QString getUserName() const;
      void setUserName(const QString& name);

   protected:
      // static QString entryName(const Common::Path& path);
      // static Common::Path pathWithoutEntryName(const Common::Path& path);

      Cache* cache; // To announce when an entry, chunk is created or deleted.
      Common::Path path; // The path to the directory containing the shared file or directory.
      Common::Hash id;

      // The name of the shared entry. Default is the directory or filename. It may be changed later by the user.
      QString userName;
   };

   /////

   class SharedDirectory : public SharedEntry
   {
   public:
      SharedDirectory(
         Cache* cache,
         const Common::Path& path,
         const Common::Hash& id = Common::Hash(),
         const QString& userName = QString()
      );

      ~SharedDirectory();

      void mergeSubSharedEntries() override;
      Directory* createSubDirs(const QStringList& names, bool physically = false);
      Entry* getRootEntry() const override;
      Common::Path getPath() const override;

      Directory* getRootDir() const;

   private:
      Directory* directory;
   };

   /////

   class SharedFile : public SharedEntry
   {
   public:
      SharedFile(
         Cache* cache,
         const Common::Path& path,
         const Common::Hash& id = Common::Hash(),
         const QString& userName = QString()
      );

      ~SharedFile();

      void mergeSubSharedEntries() override;
      // Directory* createSubDirs(const QStringList& names, bool physically = false) ;
      Entry* getRootEntry() const override;
      Common::Path getPath() const override;

      File* getRootFile() const;

   private:
      File* file;
   };
}
