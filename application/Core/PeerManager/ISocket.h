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
  
#ifndef PEERMANAGER_ISOCKET_H
#define PEERMANAGER_ISOCKET_H

#include <QAbstractSocket>

#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/Network/MessageSocket.h>

namespace PM
{
   class ISocket
   {
   public:
      enum FinishedStatus
      {
         SFS_OK,
         SFS_ERROR,
         SFS_TO_CLOSE
      };

      virtual ~ISocket() {}

      /**
        * Used by downloaders or uploaders to read or write data.
        */
      virtual QAbstractSocket* getQSocket() const = 0;

      /**
        * Returns the ID of the remote peer on which the socket is connected.
        */
      virtual Common::Hash getRemotePeerID() const = 0;

      /**
        * Used by uploader to tell when an upload is finished.
        * TODO : should be removed and only be called by the peerManager (as with downloads).
        */
      virtual void finished(FinishedStatus status = SFS_OK) = 0;
   };
}

#endif
