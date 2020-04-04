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
   protected:
      SharedEntry(Cache* cache, const Common::Path& path, const Common::Hash& id = Common::Hash());
      ~SharedEntry();

   public:
      static SharedEntry* create(Cache* cache, const QString& pathStr, const Common::Hash& id = Common::Hash());

      /**
        * Try to merge other shared entry with this one.
        * For exemple "/sharing/folder1/" can be merged with "/sharing/".
        * Should be called after each new SharedDirectory created.
        */
      virtual void mergeSubSharedEntries() = 0;
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
      virtual Common::Path getFullPath() const = 0;

      void populateEntry(Protos::Common::Entry* entry) const;

      void del(bool invokeDelete = true);

      void moveInto(Directory* directory);
      void moveInto(const QString& path);

      Cache* getCache() const;
      Common::Path getPath() const;
      Common::Hash getId() const;
      QString getUserName() const;

   private:
      static QString entryName(const Common::Path& path);
      static Common::Path pathWithoutEntryName(const Common::Path& path);

      Cache* cache; // To announce when an entry, chunk is created or deleted.
      Common::Path path; // Always a directory.
      Common::Hash id;
      QString userName; // The name of the shared entry. Default is the directory or file name. It may be changed by the user.
   };

   /////

   class SharedDirectory : public SharedEntry
   {
   public:
      SharedDirectory(Cache* cache, const Common::Path& path, const Common::Hash& id = Common::Hash());

      void mergeSubSharedEntries();
      Entry* getRootEntry() const;
      Common::Path getFullPath() const;

   private:
      Directory* directory;
   };

   /////

   class SharedFile : public SharedEntry
   {
   public:
      SharedFile(Cache* cache, const Common::Path& path, const Common::Hash& id = Common::Hash());

      void mergeSubSharedEntries();
      Entry* getRootEntry() const;
      Common::Path getFullPath() const;

   private:
      File* file;
   };
}
