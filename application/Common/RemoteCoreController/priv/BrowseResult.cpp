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
  
#include <priv/BrowseResult.h>
using namespace RCC;

#include <priv/InternalCoreConnection.h>

BrowseResult::BrowseResult(InternalCoreConnection* coreConnection, const Common::Hash& peerID, int socketTimeout) :
   IBrowseResult(socketTimeout), peerID(peerID), tag(0)
{
   this->init(coreConnection);
}

BrowseResult::BrowseResult(InternalCoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry, int socketTimeout) :
   IBrowseResult(socketTimeout), peerID(peerID), tag(0)
{
   this->browseMessage.mutable_dirs()->add_entry()->CopyFrom(entry);
   this->init(coreConnection);
}

BrowseResult::BrowseResult(InternalCoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots, int socketTimeout) :
   IBrowseResult(socketTimeout), peerID(peerID), tag(0)
{
   this->browseMessage.mutable_dirs()->CopyFrom(entries);
   this->browseMessage.set_get_roots(withRoots);
   this->init(coreConnection);
}

void BrowseResult::start()
{
   this->browseMessage.mutable_peer_id()->set_hash(this->peerID.getData(), Common::Hash::HASH_SIZE);
   this->coreConnection->send(Common::MessageHeader::GUI_BROWSE, this->browseMessage);
   this->startTimer();
}

void BrowseResult::setTag(quint64 tag)
{
   this->tag = tag;
}

void BrowseResult::browseResult(const Protos::GUI::BrowseResult& browseResult)
{
   if (browseResult.tag() == this->tag) // Is this message for us?
   {
      this->tag = 0; // To avoid multi emit (should not occurs).
      this->stopTimer();
      emit result(browseResult.entries());
   }
}

void BrowseResult::init(InternalCoreConnection* coreConnection)
{
   this->coreConnection = coreConnection;
   connect(this->coreConnection, SIGNAL(browseResult(const Protos::GUI::BrowseResult&)), this, SLOT(browseResult(const Protos::GUI::BrowseResult&)));
}
