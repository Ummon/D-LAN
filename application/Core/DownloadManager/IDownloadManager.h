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

#include <QList>
#include <QSharedPointer>

#include <Protos/common.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>

#include <Core/DownloadManager/IChunkDownloader.h>

#include <Core/PeerManager/IPeer.h>

namespace DM
{
   class IDownload;

   class IDownloadManager
   {
   public:
      virtual ~IDownloadManager() {}

      /**
        * The entry will be put in the root of the first shared directory with enough space. This shared directory is defined when the download starts.
        * @remarks 'remoteEntry.path' is not taken into account when creating the local file.
        */
      virtual void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource) = 0;

      /**
        * The entry will be put in the given path relatively to the given directory. The path directories are created if they don't exist.
        */
      virtual void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath) = 0;

      /**
        * The entry is put in the given absolute path.
        * If the last directory isn't shared it is added as a shared directory. TODO: This behavior will be changed in the 1.2 version as we can share a single file without sharing its directory.
        */
      virtual void addDownload(const Protos::Common::Entry& remoteEntry, PM::IPeer* peerSource, const QString& absolutePath) = 0;

      /**
        * @remarks The returned download pointers must not be retained.
        */
      virtual QList<IDownload*> getDownloads() const = 0;

      /**
        * Move all downloads 'downloadIDs' before or after 'downloadIDRefs' depending of 'position'.
        */
      virtual void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position) = 0;

      /**
        * Remove all completed download from the queue, will not delete any physical file.
        */
      virtual void removeAllCompleteDownloads() = 0;

      /**
        * Remove the given download IDs from the queue. Unfinished files are physically removed.
        */
      virtual void removeDownloads(QList<quint64> IDs) = 0;

      /**
        * Pause or unpause some downloads.
        */
      virtual void pauseDownloads(QList<quint64> IDs, bool pause = true) = 0;

      /**
        * Return the n (at max) first unfinished chunks. The chunks are taken from the first files in the download queue.
        */
      virtual QList<QSharedPointer<IChunkDownloader>> getTheFirstUnfinishedChunks(int n) = 0;

      /**
        * Return the oldest updated chunks. Each time we know which peer owns a chunk, this chunk is updated.
        */
      virtual QList<QSharedPointer<IChunkDownloader>> getTheOldestUnfinishedChunks(int n) = 0;

      /**
        * @return Byte/s.
        */
      virtual int getDownloadRate() = 0;
   };
}
