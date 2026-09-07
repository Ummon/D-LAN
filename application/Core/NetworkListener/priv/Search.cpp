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
  
#include <priv/Search.h>
using namespace NL;

#include <QRandomGenerator64>

#include <Common/ProtoHelper.h>
#include <Common/Network/MessageHeader.h>
#include <Common/Settings.h>

#include <priv/Log.h>

/**
  * @class NL::Search
  * @author mcuony
  * @author gburri
  */

Search::Search(UDPListener& uDPListener) :
   uDPListener(uDPListener), nbResult(0), tag(0)
{
}

quint64 Search::search(const Protos::Common::FindPattern& findPattern)
{
   if (this->tag != 0)
   {
      L_ERRO(QString("You can't launch a search twice!"));
      return 0;
   }

   Protos::Core::Find findMessage;

   quint64 tag;
   do
      tag = QRandomGenerator64::global()->generate64();
   while (tag == 0); // Zero is reserved for failure.
   findMessage.set_tag(tag);
   findMessage.mutable_pattern()->CopyFrom(findPattern);

   const auto status = this->uDPListener.send(Common::MessageHeader::CORE_FIND, findMessage);
   if (status != INetworkListener::SendStatus::OK)
   {
      L_ERRO(status == INetworkListener::SendStatus::MESSAGE_TOO_LARGE ?
         "Unable to start search: request exceeds the UDP message size limit" :
         "Unable to start search: request could not be sent");
      return 0;
   }

   this->tag = tag;
   this->nbResult = 0;
   this->timer.start();
   connect(&this->uDPListener, &UDPListener::newFindResultMessage, this, &Search::newFindResult);

   return this->tag;
}

qint64 Search::elapsed()
{
   return this->timer.isValid() ? this->timer.elapsed() : -1;
}

/**
  * Called when a result is recevied, if the tag matches, we forward the result to our listeners.
  */
void Search::newFindResult(const Protos::Common::FindResult& result)
{
   static quint32 MAX_NUMBER_RESULT = SETTINGS.get<quint32>("max_number_of_result_shown");
   if (result.tag() == this->tag && this->nbResult + static_cast<quint32>(result.entries_size()) <= MAX_NUMBER_RESULT)
   {
      this->nbResult += result.entries_size();
      emit found(result);
   }
}
