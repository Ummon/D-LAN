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

#include <priv/Cache/Entry.h>
using namespace FM;

#include <Common/KnownExtensions.h>
#include <Common/ProtoHelper.h>
#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/Directory.h>

Entry::Entry(SharedEntry* root, const QString& name, Directory* parentDirectory, qint64 size) :
   name(name), root(root), parentDirectory(parentDirectory), size(size)
{
   this->getCache()->onEntryAdded(this);
}

Entry::~Entry()
{
}

void Entry::del(bool invokeDelete)
{
   this->getCache()->onEntryRemoved(this);

   // Invoke 'deleteEntry' in the main loop.
   if (invokeDelete)
      QMetaObject::invokeMethod(this->getCache(), "deleteEntry", Qt::QueuedConnection, Q_ARG(Entry*, this));
}

void Entry::populateEntry(Protos::Common::Entry* entry, bool setSharedEntry) const
{
   if (this->parentDirectory)
   {
      // The entry name is not included.
      entry->set_path(this->getRelativePath().removeLastElement().toString().toStdString());
      entry->set_name(this->getName().toStdString());
   }

   entry->set_size(this->getSize());

   if (setSharedEntry)
      this->populateSharedEntry(entry);
}

Cache* Entry::getCache()
{
   return this->root->getCache();
}

bool Entry::isRoot() const
{
   return this->root->getRootEntry() == this;
}

SharedEntry* Entry::getRoot() const
{
   return this->root;
}

QString Entry::getName() const
{
   return this->name;
}

QString Entry::getNameWithoutExtension() const
{
   return Common::KnownExtensions::removeExtension(this->name);
}

QString Entry::getExtension() const
{
   return Common::KnownExtensions::getExtension(this->name);
}

/**
  * When a file or a directory is renamed.
  */
void Entry::rename(const QString& newName)
{
   if (this->name == newName)
      return;

   const QString oldName = this->name;
   this->name = newName;
   this->getCache()->onEntryRenamed(this, oldName);
}

void Entry::setParentDirectory(Directory* dir)
{
   if (this->parentDirectory != dir)
   {
      this->parentDirectory = dir;
      if (this->parentDirectory)
         this->setRootRecursively(this->parentDirectory->getRoot());
   }
}

qint64 Entry::getSize() const
{
   return this->size;
}

void Entry::setSize(qint64 newSize)
{
   if (newSize != this->size)
   {
      this->getCache()->onEntryResizing(this);
      qint64 oldSize = this->size;
      this->size = newSize;
      this->getCache()->onEntryResized(this, oldSize);
   }
}

void Entry::populateSharedEntry(Protos::Common::Entry* entry) const
{
   const SharedEntry* root = this->getRoot();
   if (root)
   {
      auto sharedEntry = entry->mutable_shared_entry();

      sharedEntry->mutable_id()->set_hash(root->getId().getData(), Common::Hash::HASH_SIZE);
      sharedEntry->set_path(root->getPath().toString().toStdString());
      sharedEntry->set_shared_name(root->getUserName().toStdString());
   }
}

