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
   enum Status
   {
      QUEUED = 0x1,
      GETTING_THE_HASHES = 0x2,
      DOWNLOADING = 0x3,
      COMPLETE = 0x4,
      PAUSED = 0x5,
      DELETED = 0x6,

      UNKNOWN_PEER_SOURCE = 0x10, // The source peer can't be found.
      ENTRY_NOT_FOUND = 0x11, // The source peer can't find the entry.
      NO_SOURCE = 0x12, // Some chunks can't be downloaded. Only when there is no more downloading.

      // Error status:
      NO_SHARED_DIRECTORY_TO_WRITE = 0x20,
      NO_ENOUGH_FREE_SPACE = 0x21,
      UNABLE_TO_CREATE_THE_FILE = 0x22,
      UNABLE_TO_CREATE_THE_DIRECTORY = 0x30,
      UNABLE_TO_RETRIEVE_THE_HASHES = 0x23,

      TRANSFER_ERROR = 0x24,
      UNABLE_TO_OPEN_THE_FILE = 0x25,
      FILE_IO_ERROR = 0x26,
      FILE_NON_EXISTENT = 0x27,
      GOT_TOO_MUCH_DATA = 0x28,
      HASH_MISMATCH = 0x29,

      DIRECTORY_SCANNING_IN_PROGRESS = 0x31,
      UNABLE_TO_GET_ENTRIES = 0x32
   };

   class IDownload
   {
   public:
      virtual ~IDownload() {}

      /**
        * Identify a download, useful to move or remove downloads.
        */
      virtual quint64 getID() const = 0;

      virtual Status getStatus() const = 0;

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
