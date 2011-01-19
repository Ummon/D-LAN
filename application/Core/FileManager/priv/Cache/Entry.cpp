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
  
#include "Entry.h"
using namespace FM;

#include <Common/ProtoHelper.h>
#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/FileManager.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/SharedDirectory.h>

Entry::Entry(Cache* cache, const QString& name, qint64 size) :
   cache(cache), name(name), size(size)
{
   this->cache->onEntryAdded(this);
}

Entry::~Entry()
{
   this->cache->onEntryRemoved(this);
}

void Entry::populateEntry(Protos::Common::Entry* entry, bool setSharedDir) const
{
   QString name = this->getName();

   Common::ProtoHelper::setStr(*entry, &Protos::Common::Entry::set_path, this->getPath());
   Common::ProtoHelper::setStr(*entry, &Protos::Common::Entry::set_name, name);
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

const QString& Entry::getName() const
{
   return this->name;
}

void Entry::changeName(const QString& newName)
{
   this->cache->onEntryRemoved(this);
   this->name = newName;
   this->cache->onEntryAdded(this);
}

qint64 Entry::getSize() const
{
   return this->size;
}
