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
  
#ifndef DOWNLOADMANAGER_ICHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_ICHUNKDOWNLOAD_H

#include <QObject>

#include <Common/Hash.h>
#include <Core/PeerManager/IPeer.h>

namespace DM
{
   class IChunkDownload
   {
   public:
      virtual ~IChunkDownload() {}

      /**
        * Gets the hash of the associated chunk.
        */
      virtual Common::Hash getHash() const = 0;

      /**
        * Define (or redefine) the peers which have the chunk.
        */
      virtual void addPeer(PM::IPeer* peer) = 0;
      virtual void rmPeer(PM::IPeer* peer) = 0;
   };
}
#endif
