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
  
#ifndef NETWORKLISTENER_ICHAT_H
#define NETWORKLISTENER_ICHAT_H

#include <QObject>
#include <QString>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

namespace NL
{
   class IChat : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IChat() {}

      /**
        * Send a message to everyone.
        */
      virtual void send(const QString& message) = 0;

      /**
        * Return the last received messages. The first of the list is the older and the last the younger.
        * The maximum number of messages is defined by 'Protos.Core.Settings.max_number_of_chat_message_saved'.
        */
      virtual Protos::GUI::EventChatMessages getLastMessages() const = 0;

   signals:
      /**
        * Emitted when one or more messages are received.
        */
      void newMessage(const Protos::GUI::EventChatMessages_Message&);
   };
}
#endif
