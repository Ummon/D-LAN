/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef FILEMANAGER_ENTRY_H
#define FILEMANAGER_ENTRY_H

#include <QString>

#include <Common/Uncopyable.h>

#include <Protos/common.pb.h>

namespace FM
{
   class Directory;
   class SharedDirectory;
   class Cache;

   class Entry : Common::Uncopyable
   {
   public:
      Entry(Cache* cache, const QString& name, qint64 size = 0);
      virtual ~Entry();

      virtual void populateEntry(Protos::Common::Entry* entry, bool setSharedDir = false) const;
      void populateEntrySharedDir(Protos::Common::Entry* entry) const;

      Cache* getCache();

      /**
        * Return the relative path from the root directory.
        * It's the directory in which the entry is.
        * For example : "/animals/fish/"
        * An entry in a root will have a path like "/".
        * A root (SharedDirectory) will have an empty path ("").
        */
      virtual QString getPath() const = 0;

      /**
        * Return the full absolute path to the entry.
        * Directories always end with a '/'.
        */
      virtual QString getFullPath() const = 0;

      virtual SharedDirectory* getRoot() const = 0;

      virtual void removeUnfinishedFiles() = 0;

      QString getName() const;
      virtual void changeName(const QString& newName);

      virtual qint64 getSize() const;

   protected:
      Cache* cache;

      QString name;
      qint64 size;
   };

   bool operator<(const Entry& e1, const Entry& e2);
   bool operator>(const Entry& e1, const Entry& e2);
}
#endif
