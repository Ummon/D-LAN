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
  
#include <priv/DirDownload.h>
using namespace DM;

#include <Common/ProtoHelper.h>

#include <priv/Log.h>

/**
  * @class DM::DirDownload
  *
  * A DirDownload will try, when the method 'retrieveEntries' is called, to retreive all the sub entries of a remote directory.
  * The sub entries can be a mix of files and directories.
  * Once the content is know, the signal 'newEntries' is emmited.
  */

DirDownload::DirDownload(
   OccupiedPeers& occupiedPeersAskingForEntries,
   PM::IPeer* peerSource,
   const Protos::Common::Entry& remoteEntry,
   const Protos::Common::Entry& localEntry
) :
   Download(peerSource, remoteEntry, localEntry),
   occupiedPeersAskingForEntries(occupiedPeersAskingForEntries)
{
   L_DEBU(QString("New DirDownload : source = %1, remoteEntry : \n%2\nlocalEntry : \n%3").
      arg(this->peerSource->toStringLog()).
      arg(Common::ProtoHelper::getDebugStr(this->remoteEntry)).
      arg(Common::ProtoHelper::getDebugStr(this->localEntry))
   );
}

DirDownload::~DirDownload()
{
   this->setStatus(DELETED);

   this->occupiedPeersAskingForEntries.setPeerAsFree(this->peerSource);

   this->getEntriesResult.clear();
}

void DirDownload::start()
{
   this->retrieveEntries();
}

/**
  * Ask the DirDownload to get its content.
  * The signal 'newEntries' will be emitted when the answer is received.
  */
bool DirDownload::retrieveEntries()
{
   if (!this->hasAValidPeer() || this->status == DELETED || !this->occupiedPeersAskingForEntries.setPeerAsOccupied(this->peerSource))
      return false;

   Protos::Core::GetEntries getEntries;
   getEntries.mutable_dirs()->add_entry()->CopyFrom(this->remoteEntry);
   this->getEntriesResult = this->peerSource->getEntries(getEntries);
   connect(this->getEntriesResult.data(), SIGNAL(result(const Protos::Core::GetEntriesResult&)), this, SLOT(result(const Protos::Core::GetEntriesResult&)));
   connect(this->getEntriesResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->getEntriesResult->start();

   return true;
}

bool DirDownload::updateStatus()
{
   if (Download::updateStatus())
      return true;

   if (!this->peerSource->isAvailable())
      this->setStatus(UNKNOWN_PEER_SOURCE);
   else
      this->setStatus(QUEUED);

   return false;
}

void DirDownload::result(const Protos::Core::GetEntriesResult& entries)
{
   Protos::Common::Entries entriesCopy;

   // We asked for one directory, we shouldn't have zero result.
   if (entries.entries_size() > 0)
   {
      // We need to specify the shared directory for each entry.
      entriesCopy.CopyFrom(entries.entries(0)); // We take the first one which should be the only set of entries.
      for (int i = 0; i < entriesCopy.entry_size(); i++)
         entriesCopy.mutable_entry(i)->mutable_shared_dir()->CopyFrom(this->remoteEntry.shared_dir());
   }

   this->getEntriesResult.clear();

   emit newEntries(entriesCopy);
}

void DirDownload::resultTimeout()
{
   L_DEBU("Unable to retrieve the entries : timeout");
   this->getEntriesResult.clear();
   this->occupiedPeersAskingForEntries.setPeerAsFree(this->peerSource);
}
