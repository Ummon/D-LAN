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
  
#pragma once

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

namespace FM
{
   class IGetHashesResult : public QObject
   {
      Q_OBJECT
   public:
      // Create, start, and destroy requests on their owning thread.
      virtual ~IGetHashesResult() {}
      // Call on the request's owning thread. Repeated calls return the original
      // response without restarting the stream or resending hashes.
      virtual Protos::Core::GetHashesResult start() = 0;

   signals:
      /**
        * Use Qt::QueuedConnection when the receiver must process start()'s return
        * value first: hashes already known can be emitted before start() returns.
        * Direct receivers may reenter start() or destroy the request.
        * Notifications are delivered on the request's owning thread.
        */
      void nextHash(Protos::Core::HashResult hash);
   };
}
