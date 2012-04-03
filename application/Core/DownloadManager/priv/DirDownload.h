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
  
#ifndef DOWNLOADMANAGER_DIRDOWNLOAD_H
#define DOWNLOADMANAGER_DIRDOWNLOAD_H

#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetEntriesResult.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <priv/Download.h>
#include <priv/OccupiedPeers.h>

namespace DM
{
   class DirDownload : public Download
   {
      Q_OBJECT
   public:
      DirDownload(
         OccupiedPeers& occupiedPeersAskingForEntries,
         PM::IPeer* peerSource,
         const Protos::Common::Entry& remoteEntry,
         const Protos::Common::Entry& localEntry
      );
      ~DirDownload();

      void start();

      bool retrieveEntries();

   public slots:
      bool updateStatus();

   signals:
      /**
        * Emitted when the content of the folder has been retrieved.
        */
      void newEntries(const Protos::Common::Entries& entries);

   private slots:
      void result(const Protos::Core::GetEntriesResult& entries);
      void resultTimeout();

   private:
      OccupiedPeers& occupiedPeersAskingForEntries;
      QSharedPointer<PM::IGetEntriesResult> getEntriesResult;
   };
}
#endif
