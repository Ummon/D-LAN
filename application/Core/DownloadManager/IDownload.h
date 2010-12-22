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
  
#ifndef DOWNLOADMANAGER_IDOWNLOAD_H
#define DOWNLOADMANAGER_IDOWNLOAD_H

#include <QObject>
#include <QSet>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace DM
{
   enum Status
   {
     QUEUED = 0x1,
     INITIALIZING = 0x2,
     DOWNLOADING = 0x3,
     COMPLETE = 0x4,
     PAUSED = 0x5,
     DELETED = 0x6,

     // All theses status will imply the paused status.
     UNKNOWN_PEER = 0x10, // The source peer can't be found.
     ENTRY_NOT_FOUND = 0x11, // The source peer can't find the entry.
     NO_SOURCE = 0x12, // Some chunk can't be downloaded. Only when there is no more downloading.

     // Error status :
     NO_SHARED_DIRECTORY_TO_WRITE = 0x20,
     NO_ENOUGH_FREE_SPACE = 0x21,
     UNABLE_TO_CREATE_THE_FILE = 0x22,
     UNABLE_TO_RETRIEVE_THE_HASHES = 0x23,
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

      /**
        * Is a status is erroneous? See the enum 'Status' for more information.
        */
      virtual bool isStatusErroneous() const = 0;

      /**
        * Return a value between 0 and 100.
        */
      virtual int getProgress() const = 0;

      virtual Common::Hash getPeerSourceID() const = 0;

      /**
        * Return all the peer who own at least one chunk.
        */
      virtual QSet<Common::Hash> getPeers() const = 0;

      /**
        * Return the associated entry to the download, it contains :
        * - The type (directory or file)
        * - The path
        * - The name
        * - The size
        * The entry can be remote or local, once the download is completed (see 'getStatus')
        * the remote becomes local and the path corresponds to the local file.
        */
      virtual const Protos::Common::Entry& getEntry() = 0;

      /**
        * Stop and delete the download.
        * If the download has begun the downloaded data are removed.
        */
      virtual void remove() = 0;
   };
}
#endif
