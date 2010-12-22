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
  
#include <priv/DirDownload.h>
using namespace DM;

#include <Common/ProtoHelper.h>

#include <priv/Log.h>

DirDownload::DirDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry), retrieveEntriesOK(false)
{
   L_DEBU(QString("New DirDownload : path = %1%2 source = %3").
      arg(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path)).
      arg(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name)).
      arg(this->peerSourceID.toStr())
   );
}

DirDownload::~DirDownload()
{
   this->status = DELETED;
   this->getEntriesResult.clear();
}

QSet<Common::Hash> DirDownload::getPeers() const
{
   return QSet<Common::Hash>() << this->peerSourceID;
}

/**
  * Ask the DirDownload to get its content.
  * The signal 'newEntries' will be emitted when the answer is received.
  */
bool DirDownload::retrieveEntries()
{
   this->retrieveEntriesOK = true;

   if (!this->hasAValidPeer())
      return false;

   Protos::Core::GetEntries getEntries;
   getEntries.mutable_dirs()->add_entry()->CopyFrom(this->entry);
   this->getEntriesResult = this->peerSource->getEntries(getEntries);
   connect(this->getEntriesResult.data(), SIGNAL(result(const Protos::Core::GetEntriesResult&)), this, SLOT(result(const Protos::Core::GetEntriesResult&)));
   connect(this->getEntriesResult.data(), SIGNAL(timeout()), this, SLOT(resultTimeout()));
   this->getEntriesResult->start();

   return true;
}

void DirDownload::retrievePeer()
{
   Download::retrievePeer();

   if (this->retrieveEntriesOK)
      this->retrieveEntries();
}

void DirDownload::result(const Protos::Core::GetEntriesResult& entries)
{
   this->getEntriesResult.clear(); // Is the 'IGetEntriesResult' object is deleted? Must we disconnect the signal? Answer : No the signal is automatically disconnected.

   if (entries.entries_size() == 0)
      return;

   // We need to specify the shared directory for each entry.
   Protos::Common::Entries entriesCopy(entries.entries(0));
   for (int i = 0; i < entriesCopy.entry_size(); i++)
      entriesCopy.mutable_entry(i)->mutable_shared_dir()->CopyFrom(this->entry.shared_dir());

   emit newEntries(entriesCopy);
}

void DirDownload::resultTimeout()
{
   this->getEntriesResult.clear();
   this->retrieveEntries();
}

