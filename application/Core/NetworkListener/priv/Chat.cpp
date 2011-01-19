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
  
#include <priv/Chat.h>
using namespace NL;

#include <Common/ProtoHelper.h>
#include <Common/Network.h>
#include <Common/LogManager/Builder.h>

#include <priv/UDPListener.h>

/**
  * @class Chat
  * @author mcuony
  * @author gburri
  */

Chat::Chat(UDPListener& uDPListener) :
   uDPListener(uDPListener)
{
   // Listening for new messages and forward them to our own signal.
   Chat::connect(
      &this->uDPListener,
      SIGNAL(newChatMessage(const Common::Hash&, const Protos::Core::ChatMessage&)),
      this,
      SIGNAL(newMessage(const Common::Hash&, const Protos::Core::ChatMessage&))
   );
}

void Chat::send(const QString& message)
{
   Protos::Core::ChatMessage chatMessage;
   Common::ProtoHelper::setStr(chatMessage, &Protos::Core::ChatMessage::set_message, message);

   this->uDPListener.send(Common::Network::CORE_CHAT_MESSAGE, chatMessage);
}
