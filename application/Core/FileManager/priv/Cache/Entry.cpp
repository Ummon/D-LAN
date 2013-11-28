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

#include <Common/ProtoHelper.h>
#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/SharedDirectory.h>

Entry::Entry(Cache* cache, const QString& name, qint64 size) :
   cache(cache), name(name), size(size), mutex(QMutex::Recursive)
{
   this->cache->onEntryAdded(this);
}

Entry::~Entry()
{
}

void Entry::del(bool invokeDelete)
{
   this->cache->onEntryRemoved(this);

   if (invokeDelete)
      QMetaObject::invokeMethod(this->cache, "deleteEntry", Qt::QueuedConnection, Q_ARG(Entry*, this));
}

void Entry::populateEntry(Protos::Common::Entry* entry, bool setSharedDir) const
{
   Common::ProtoHelper::setStr(*entry, &Protos::Common::Entry::set_path, this->getPath());
   Common::ProtoHelper::setStr(*entry, &Protos::Common::Entry::set_name, this->getName());
   entry->set_size(this->getSize());

   if (setSharedDir)
      this->populateEntrySharedDir(entry);
}

void Entry::populateEntrySharedDir(Protos::Common::Entry* entry) const
{
   SharedDirectory* dir = dynamic_cast<SharedDirectory*>(this->getRoot());
   if (dir)
   {
      entry->mutable_shared_dir()->mutable_id()->set_hash(dir->getId().getData(), Common::Hash::HASH_SIZE);
      Common::ProtoHelper::setStr(*entry->mutable_shared_dir(), &Protos::Common::SharedDir::set_shared_name, dir->getName());
   }
}

Cache* Entry::getCache()
{
   return this->cache;
}

QString Entry::getName() const
{
   return this->name;
}

QString Entry::getNameWithoutExtension() const
{
   int i = this->getBeginingExtension();
   if (i != -1)
      return this->name.left(i - 1);
   else
      return this->name;
}

QString Entry::getExtension() const
{
   int i = this->getBeginingExtension();
   if (i != -1)
      return this->name.right(this->name.length() - i);
   else
      return QString();
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
   this->cache->onEntryRenamed(this, oldName);
}

qint64 Entry::getSize() const
{
   return this->size;
}
