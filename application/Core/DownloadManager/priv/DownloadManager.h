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
  
#ifndef DOWNLOADMANAGER_DOWNLOADMANAGER_H
#define DOWNLOADMANAGER_DOWNLOADMANAGER_H

#include <QList>
#include <QSet>
#include <QSharedPointer>
#include <QTimer>

#include <Common/TransferRateCalculator.h>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IDownloadManager.h>
#include <priv/OccupiedPeers.h>
#include <priv/Log.h>

namespace PM
{
   class IPeer;
}

namespace DM
{
   class Download;
   class FileDownload;

   class DownloadManager : public QObject, public IDownloadManager
   {
      Q_OBJECT
   public:
      DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);
      ~DownloadManager();

      void addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource);
      void addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& relativePath);

      Download* addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, bool complete);
      Download* addDownload(const Protos::Common::Entry& remoteEntry, const Common::Hash& peerSource, const Common::Hash& destinationDirectoryID, const QString& localRelativePath, bool complete, QMutableListIterator<Download*>& iterator);

      Download* addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, const Common::Hash& peerSource, bool complete);
      Download* addDownload(const Protos::Common::Entry& remoteEntry, const Protos::Common::Entry& localEntry, const Common::Hash& peerSource, bool complete,  QMutableListIterator<Download*>& iterator);

      QList<IDownload*> getDownloads() const;
      void moveDownloads(quint64 downloadIDRef, bool moveBefore, const QList<quint64>& downloadIDs);
      QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n) const;

      int getDownloadRate();

   private slots:
      void peerBecomesAlive(PM::IPeer* peer);

      void fileCacheLoaded();

      void newEntries(const Protos::Common::Entries& remoteEntries);

      void downloadDeleted(Download* download);

      void peerNoLongerAskingForHashes(PM::IPeer* peer);
      void peerNoLongerDownloadingChunk(PM::IPeer* peer);

      void scanTheQueueToRetrieveEntries();
      void scanTheQueue();
      void chunkDownloadFinished();

   private:
      void loadQueueFromFile();

   private slots:
      void saveQueueToFile();
      void setQueueChanged();

   private:
      bool isEntryAlreadyQueued(const Protos::Common::Entry& localEntry, const Common::Hash& peerSource);

      LOG_INIT_H("DownloadManager");

      const int NUMBER_OF_DOWNLOADER;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      OccupiedPeers occupiedPeersAskingForHashes;
      OccupiedPeers occupiedPeersDownloadingChunk;

      QList<Download*> downloads;

      int numberOfDownload;

      bool retrievingEntries; // TODO : if the socket is closed then retrievingEntries = false

      QTimer rescanTimer; // When a download has an error status, the queue will be rescaned periodically.

      QTimer saveTimer; // To know when to save the queue, for exemple each 5min.
      bool queueChanged;

     Common::TransferRateCalculator transferRateCalculator;
   };
}
#endif
