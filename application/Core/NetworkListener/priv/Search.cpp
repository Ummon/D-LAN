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
  
#include <priv/Search.h>
using namespace NL;

#include <Common/ProtoHelper.h>
#include <Common/Network.h>

#include <priv/Log.h>

/**
  * @class Search
  * @author mcuony
  * @author gburri
  */

Search::Search(UDPListener& uDPListener)
   : uDPListener(uDPListener), tag(0)
{
}

quint64 Search::search(const QString& words)
{
   if (this->tag != 0)
   {
      L_ERRO(QString("You can't launch a search twice! : %1").arg(words));
      return 0;
   }

   this->timer.start();

   Protos::Core::Find findMessage;

   this->tag = (static_cast<quint64>(this->mtrand.randInt()) << 32) | this->mtrand.randInt();
   findMessage.set_tag(this->tag);

   Common::ProtoHelper::setStr(findMessage, &Protos::Core::Find::set_pattern, words);

   connect(&this->uDPListener, SIGNAL(newFindResultMessage(Protos::Common::FindResult)), this, SLOT(newFindResult(Protos::Common::FindResult)));

   this->uDPListener.send(Common::Network::CORE_FIND, findMessage);

   return this->tag;
}

qint64 Search::elapsed()
{
   return timer.elapsed();
}

/**
  * Called when a result is recevied, if the tag matches, we forward the result to our listeners.
  */
void Search::newFindResult(const Protos::Common::FindResult& result)
{
   if (result.tag() == this->tag)
      emit found(result);
}
