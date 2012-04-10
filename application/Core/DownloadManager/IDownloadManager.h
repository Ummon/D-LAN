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
  
#ifndef DOWNLOADMANAGER_IDOWNLOADMANAGER_H
#define DOWNLOADMANAGER_IDOWNLOADMANAGER_H

#include <QList>
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <Core/DownloadManager/IChunkDownload.h>

#include <Core/PeerManager/IPeer.h>

namespace DM
{
   class IDownload;

   class IDownloadManager
   {
   public:
      virtual ~IDownloadManager() {}

      /**
        * @remarks entry.path is not taken into account .
        * The entry will be put in the root of the first shared directory with enough space.
        */
      virtual void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource) = 0;

      virtual void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath) = 0;

      virtual void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const QString& absolutePath) = 0;

      /**
        * @remarks The returned download pointers must not be retained.
        */
      virtual QList<IDownload*> getDownloads() const = 0;

      /**
        * Move all downloads 'downloadIDs' before or after 'downloadIDRefs' depending of 'position'.
        */
      virtual void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position) = 0;

      virtual void removeAllCompleteDownloads() = 0;

      virtual void removeDownloads(QList<quint64> IDs) = 0;

      /**
        * Pause or unpause some downloads.
        */
      virtual void pauseDownloads(QList<quint64> IDs, bool pause = true) = 0;

      /**
        * Return the n (at max) first unfinished chunks.
        */
      virtual QList< QSharedPointer<IChunkDownload> > getTheFirstUnfinishedChunks(int n) = 0;

      /**
        * Return the oldest updated chunks. Each time we know which peer owns a chunk, this chunk is updated.
        */
      virtual QList< QSharedPointer<IChunkDownload> > getTheOldestUnfinishedChunks(int n) = 0;

      /**
        * @return Byte/s.
        */
      virtual int getDownloadRate() = 0;
   };
}
#endif
