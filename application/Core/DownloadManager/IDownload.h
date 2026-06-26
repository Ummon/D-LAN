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
#include <QSet>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

#include <Core/PeerManager/IPeer.h>

namespace DM
{
   class IDownload
   {
   public:
      virtual ~IDownload() {}

      /**
        * Identify a download, useful to move or remove downloads.
        */
      virtual quint64 getID() const = 0;

      virtual Protos::Common::DownloadStatus getStatus() const = 0;

      virtual quint64 getDownloadedBytes() const = 0;

      virtual PM::IPeer* getPeerSource() const = 0;

      /**
        * Return all the peer who own at least one chunk. It includes the peer source, see 'getPeerSource()'.
        */
      virtual QSet<PM::IPeer*> getPeers() const = 0;

      /**
        * Return the associated entry to the download, it contains :
        * - The type (directory or file)
        * - The path. May not be defined if the path hasn't been defined when queued and the download hasn't begun.
        * - The name
        * - The size
        */
      virtual const Protos::Common::Entry& getLocalEntry() const = 0;
   };
}
