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
  
#include <priv/SearchResult.h>
using namespace RCC;

#include <QRandomGenerator64>

#include <Protos/gui_protocol.pb.h>

#include <priv/InternalCoreConnection.h>

SearchResult::SearchResult(InternalCoreConnection* coreConnection, const Protos::Common::FindPattern& findPattern, bool local, int socketTimeout) :
   ISearchResult(socketTimeout), coreConnection(coreConnection), findPattern(findPattern), local(local), tag(QRandomGenerator64::global()->generate64())
{
   connect(this->coreConnection.data(), &InternalCoreConnection::searchResult, this, &SearchResult::searchResult);
   connect(coreConnection, &InternalCoreConnection::disconnected, this, [this] {
      this->coreConnection.clear();
      this->receivingResults = false;
   });
}

void SearchResult::start()
{
   if (this->started)
      return;
   this->started = true;
   this->startTimer();
   if (!this->coreConnection || !this->coreConnection->isConnected())
      return;

   Protos::GUI::Search search;
   search.mutable_pattern()->CopyFrom(this->findPattern);
   search.set_local(this->local);
   search.set_tag(this->tag);
   this->receivingResults = true;
   this->coreConnection->send(Common::MessageHeader::GUI_SEARCH, search);
}

void SearchResult::searchResult(const Protos::Common::FindResult& findResult)
{
   if (this->receivingResults && !this->isTimedout() && findResult.tag() == this->tag) // Is this message for us?
   {
      this->stopTimer();
      emit result(findResult);
   }
}
