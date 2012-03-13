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
  
#include <priv/Download.h>
using namespace DM;

#include <Common/ProtoHelper.h>

#include <priv/Constants.h>
#include <priv/Log.h>

quint64 Download::currentID(1);

Download::Download(
   PM::IPeer* peerSource,
   const Protos::Common::Entry& remoteEntry,
   const Protos::Common::Entry& localEntry
) :
   ID(currentID++), peerSource(peerSource), remoteEntry(remoteEntry), localEntry(localEntry), status(QUEUED)
{
}

Download::~Download()
{
   L_DEBU(QString("Download deleted: %1%2")
      .arg(Common::ProtoHelper::getStr(this->localEntry, &Protos::Common::Entry::path))
      .arg(Common::ProtoHelper::getStr(this->localEntry, &Protos::Common::Entry::name))
   );
}

void Download::populateRemoteEntry(Protos::Queue::Queue_Entry* entry) const
{
   entry->mutable_remote_entry()->CopyFrom(this->remoteEntry);
}

void Download::populateLocalEntry(Protos::Queue::Queue_Entry* entry) const
{
   entry->mutable_local_entry()->CopyFrom(this->localEntry);
   entry->set_complete(this->status == COMPLETE);
}

quint64 Download::getID() const
{
   return this->ID;
}

Status Download::getStatus() const
{
   return this->status;
}

bool Download::isStatusErroneous() const
{
   return this->status >= 0x20;
}

int Download::getProgress() const
{
   return 0;
}

PM::IPeer* Download::getPeerSource() const
{
   return this->peerSource;
}

QSet<Common::Hash> Download::getPeers() const
{
   return QSet<Common::Hash>();
}

const Protos::Common::Entry& Download::getRemoteEntry() const
{
   return this->remoteEntry;
}

const Protos::Common::Entry& Download::getLocalEntry() const
{
   return this->localEntry;
}

void Download::setAsDeleted()
{
   this->status = DELETED;
}

void Download::remove()
{
   delete this;
}

bool Download::updateStatus()
{
   if (this->status == DELETED || this->status == COMPLETE || this->status == PAUSED)
      return true;

   if (!this->peerSource->isAvailable())
      this->setStatus(UNKNOWN_PEER_SOURCE);
   else
      this->setStatus(QUEUED);

   return false;
}

void Download::setStatus(Status newStatus)
{
   this->status = newStatus;
}

bool Download::hasAValidPeer()
{
   return this->peerSource->isAlive();
}
