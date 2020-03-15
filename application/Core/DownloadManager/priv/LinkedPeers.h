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

#include <QMap>
#include <QList>

#include <Core/PeerManager/IPeer.h>

#include <priv/Log.h>

namespace DM
{
   /**
     * @class LinkedPeers
     * Count the number of chunks that each peer owns. If a peer has no chunk he is not referenced.
     */
   class LinkedPeers : private QMap<PM::IPeer*, quint32>
   {
   public:
      inline QList<PM::IPeer*> getPeers()
      {
         return this->keys();
      }

      inline void addLink(PM::IPeer* peer)
      {
         quint32& n = (*this)[peer];
         n++;
         // L_DEBU(QString("addLink(..): peer: %1, n = %2").arg(peer->toStringLog()).arg(n));
      }

      inline void rmLink(PM::IPeer* peer)
      {
         quint32& n = (*this)[peer];
         n--;
         if (n == 0)
            this->remove(peer);
         // L_DEBU(QString("rmLink(..): peer: %1, n = %2").arg(peer->toStringLog()).arg(n));
      }
   };
}
