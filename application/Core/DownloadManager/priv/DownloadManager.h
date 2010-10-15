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

   class DownloadManager : public QObject, public IDownloadManager
   {
      Q_OBJECT
   public:
      DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);

      void addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry);
      void addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry, QMutableLinkedListIterator<Download*> iterator);

      QList<IDownload*> getDownloads();
      QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n);

      int getDownloadRate() const;

   private slots:
      void newEntries(const Protos::Core::GetEntriesResult& entries);

      void peerNoLongerAskingForHashes(PM::IPeer* peer);
      void peerNoLongerDownloadingChunk(PM::IPeer* peer);

      void downloadFinished();

   private:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      OccupiedPeers occupiedPeersAskingForHashes;
      OccupiedPeers occupiedPeersDownloadingChunk;

      QLinkedList<Download*> downloads;

      int numberOfDownload;
   };
}
#endif
