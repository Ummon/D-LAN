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
  
#ifndef DOWNLOADMANAGER_CONSTANTS_H
#define DOWNLOADMANAGER_CONSTANTS_H

#include <QString>

namespace DM
{
   const int CHECK_DEAD_PEER_PERIOD = 10000; // [ms]. TODO : use the signal IPeerManager::peerBecomesAlive(..) instead of checking continuously.
   const int RETRY_PEER_GET_HASHES_PERIOD = 10000; // [ms]. If the hashes cannot be retrieve frome a peer, we wait 10s before retrying.
   const int RESCAN_QUEUE_PERIOD_IF_ERROR = 10000; // [ms]. If one or more download has a status >= 0x20 then all the queue will be periodically rescaned.

   // 2 -> 3 : BLAKE -> Sha-1
   const int FILE_QUEUE_VERSION = 3;
}

#endif
