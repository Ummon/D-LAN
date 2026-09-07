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

#include <atomic>

#include <QString>
#include <QRecursiveMutex>
#include <QMutex>
#include <QSharedPointer>

#include <Common/Uncopyable.h>
#include <Common/Path.h>

#include <Protos/common.pb.h>

#include <priv/SizeIndex.h>

namespace FM
{
   class Directory;
   class SharedEntry;
   class Cache;

   class Entry : public ISizeItem, Common::Uncopyable
   {
   protected:
      Entry(
         SharedEntry* root,
         const QString& name,
         Directory* parentDirectory = nullptr,
         qint64 size = 0,
         bool hidden = false
      );

   public:
      virtual ~Entry();

      /**
        * Detaches the entry from the cache and schedules its deletion.
        * Calling it more than once is harmless: only the first call has an effect.
        */
      virtual void del(bool invokeDelete = true);

      virtual void populateEntry(Protos::Common::Entry* entry, bool setSharedEntry = false) const;

      Cache* getCache();

      /**
        * Returns the relative path from the root directory.
        */
      virtual Common::Path getRelativePath() const = 0;

      /**
        * Returns the full absolute path of the entry.
        */
      virtual Common::Path getAbsolutePath() const = 0;

      /**
        * Returns the entry matching the given path, it can be 'this'.
        */
      virtual Entry* getEntry(const Common::Path& path) = 0;

      virtual void removeUnfinishedFiles() = 0;

      /**
        * Move the entry to a cached directory.
        */
      virtual void moveInto(Directory* directory) = 0;

      bool isRoot() const;
      SharedEntry* getRoot() const;

      QString getName() const;
      QString getUserName() const;
      QString getNameWithoutExtension() const;

      virtual void rename(const QString& newName);

      void setParentDirectory(Directory* dir);

      virtual qint64 getSize() const override;
      virtual uint hash() const override { return qHash(this); }

      virtual void setSize(qint64 newSize);

      void setHidden(bool hidden);

      int getDepth() const;

      void populateSharedEntry(Protos::Common::Entry* entry) const;

   protected:
      virtual void setRootRecursively(SharedEntry* sharedEntry) = 0;

      // Writers still use mutex to serialize tree/index transitions. Readers used by
      // parent containers and search indexes must never acquire a child's mutex.
      void setName(const QString& name);

   private:
      // Leaf lock: copy/replace only; release before any callback or other lock.
      mutable QMutex nameMutex;
      QString name;

   protected:
      // Individual snapshots, not lifetime ownership or a transaction over the tree.
      std::atomic<SharedEntry*> root;
      std::atomic<Directory*> parentDirectory; // Can be null if root.
      std::atomic<qint64> size;
      std::atomic<bool> hidden { false };

      std::atomic<bool> deletePending { false }; ///< Set by 'del()', the deletion may be queued in another thread.

      // Chunks retain the file's mutex so it remains valid while they wait for detachment,
      // even after the owning entry has been destroyed.
      const QSharedPointer<QRecursiveMutex> mutexStorage = QSharedPointer<QRecursiveMutex>::create();
      QRecursiveMutex& mutex = *mutexStorage;
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
