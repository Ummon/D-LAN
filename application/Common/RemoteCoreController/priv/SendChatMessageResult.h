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
  
#ifndef RCC_SENDCHATMESSAGERESULT_H
#define RCC_SENDCHATMESSAGERESULT_H

#include <QString>
#include <QList>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <ISendChatMessageResult.h>

namespace RCC
{
   class InternalCoreConnection;

   class SendChatMessageResult : public ISendChatMessageResult
   {
      Q_OBJECT
   public:
      SendChatMessageResult(InternalCoreConnection* coreConnection, int socketTimeout, const QString& message, const QString& roomName, const QList<Common::Hash>& peerIDsAnswered);

      void start();

      void setResult(const Protos::GUI::ChatMessageResult& result);

   private:
      InternalCoreConnection* coreConnection;

      QString message;
      QString roomName;
      QList<Common::Hash> peerIDsAnswered;
   };
}

#endif
