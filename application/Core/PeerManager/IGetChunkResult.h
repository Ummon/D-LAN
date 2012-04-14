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
  
#ifndef PEERMANAGER_IGET_CHUNK_RESULT_H
#define PEERMANAGER_IGET_CHUNK_RESULT_H

#include <QObject>
#include <QIODevice>
#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <Common/Timeoutable.h>

#include <Core/PeerManager/ISocket.h>

namespace PM
{
   class IGetChunkResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      IGetChunkResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~IGetChunkResult() {}
      virtual void start() = 0;

      /**
        * Never call this method, only for internal purpose.
        */
      virtual void doDeleteLater() = 0;

      /**
        * If there is an error during the streaming, it can be reported by calling this method.
        */
      virtual void setStatus(bool closeTheSocket) = 0;

   signals:
      void result(const Protos::Core::GetChunkResult& result);
      void stream(QSharedPointer<PM::ISocket> socket);
   };
}
#endif
