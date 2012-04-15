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
  
#ifndef PEERMANAGER_GET_HASHES_RESULT_H
#define PEERMANAGER_GET_HASHES_RESULT_H

#include <QObject>
#include <QSharedPointer>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Network/MessageHeader.h>
#include <Common/Uncopyable.h>

#include <IGetHashesResult.h>
#include <priv/PeerMessageSocket.h>

namespace PM
{
   class GetHashesResult : public IGetHashesResult, Common::Uncopyable
   {
      Q_OBJECT
   public:
      GetHashesResult(const Protos::Common::Entry& file, QSharedPointer<PeerMessageSocket> socket);
      void start();
      void doDeleteLater();

   private slots:
      void newMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);

   private:
      const Protos::Common::Entry file;
      QSharedPointer<PeerMessageSocket> socket;
   };
}

#endif
