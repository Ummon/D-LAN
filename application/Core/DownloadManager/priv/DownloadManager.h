#ifndef DOWNLOADMANAGER_DOWNLOADMANAGER_H
#define DOWNLOADMANAGER_DOWNLOADMANAGER_H

#include <QLinkedList>
#include <QMutableLinkedListIterator>
#include <QList>
#include <QSet>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IDownloadManager.h>
#include <priv/OccupiedPeers.h>

namespace PM
{
   class IPeer;
}

namespace DM
{
   class Download;
   class FileDownload;
   class ChunkDownloader;

   class DownloadManager : public IDownloadManager
   {
      Q_OBJECT
   public:
      DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);

      void addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry);
      void addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry, QMutableLinkedListIterator<Download*> iterator);

      QList<IDownload*> getDownloads();
      QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n);

   private slots:
      void newEntries(const Protos::Core::GetEntriesResult& entries);

      void peerNoLongerAskingForHashes(PM::IPeer* peer);
      void peerNoLongerDownloadingChunk(PM::IPeer* peer);

   private:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      OccupiedPeers occupiedPeersAskingForHashes;
      OccupiedPeers occupiedPeersDownloadingChunk;

      QLinkedList<Download*> downloads;
      QList<ChunkDownloader*> chunkDownloaders;

      bool scanFlag; // When a peer is no more occupied to download we set this flag to true, it means that when a Downloader is idle we can do a scan to search a chunk to download.
   };
}
#endif
