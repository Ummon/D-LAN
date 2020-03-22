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
#include <QMutex>

#include <Common/Uncopyable.h>
#include <Common/Path.h>

#include <Protos/common.pb.h>

namespace FM
{
   class Directory;
   class SharedEntry;
   class Cache;

   class Entry : Common::Uncopyable
   {
   protected:
      Entry(SharedEntry* root, const QString& name, qint64 size = 0);

   public:
      virtual ~Entry();
      virtual void del(bool invokeDelete = true);

      virtual void populateEntry(Protos::Common::Entry* entry, bool setSharedEntry = false) const;
      void populateSharedEntry(Protos::Common::Entry* entry) const;

      Cache* getCache();

      /**
        * Return the relative path from the root directory.
        * It's the directory in which the entry is.
        * For example : "/animals/fish/"
        * An entry in a root will have a path like "/".
        * A root (SharedEntry) will have an empty path ("").
        */
      virtual Common::Path getPath() const = 0;

      /**
        * Return the full absolute path to the entry.
        * Directories always end with a '/'.
        */
      virtual Common::Path getFullPath() const = 0;

      virtual void removeUnfinishedFiles() = 0;

      virtual void moveInto(Directory* directory) = 0;

      SharedEntry* getRoot() const;
      QString getName() const;
      QString getNameWithoutExtension() const;
      QString getExtension() const;
      virtual void rename(const QString& newName);

      qint64 getSize() const;
      void setSize(qint64 newSize);

   protected:
      QString name;

   private:
      SharedEntry* root;
      qint64 size;

   protected:
      mutable QMutex mutex;
   };

   inline bool operator<(const Entry& e1, const Entry& e2)
   {
      return e1.getName().toLower() < e2.getName().toLower();
   }

   inline bool operator>(const Entry& e1, const Entry& e2)
   {
      return e1.getName().toLower() > e2.getName().toLower();
   }

   inline uint qHash(const Entry* entry)
   {
      uint h = 0;
      static const int n = sizeof(Entry*) > sizeof(uint) ? sizeof(Entry*) / sizeof(uint) : 1;
      for (int i = 0; i < n; ++i)
         h ^= intptr_t(entry) >> (i * 8 * sizeof(uint));
      return h;
   }
}
