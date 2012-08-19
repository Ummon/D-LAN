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
  
#ifndef CHATSYSTEM_CHATMESSAGE_H
#define CHATSYSTEM_CHATMESSAGE_H

#include <QString>
#include <QDateTime>

#include <Libs/MersenneTwister.h>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace CS
{
   class ChatMessage
   {
   public:
      ChatMessage(const QString& message, const Common::Hash& ownerID, const QString& ownerNick);
      ChatMessage(const Protos::Common::ChatMessage& chatMessage);

      quint64 getID() const;
      QDateTime getTime() const;

      void fillProtoChatMessage(Protos::Common::ChatMessage& protoChatMessage) const;

   private:
      static MTRand mtrand;

      const quint64 ID;
      const QString message;
      const Common::Hash ownerID;
      const QDateTime time; // UTC.
      const QString ownerNick;
      const QString room; // Empty if the message belongs to the main chat.
   };
}

#endif
