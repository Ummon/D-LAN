#ifndef DOWNLOADMANAGER_FILEDOWNLOAD_H
#define DOWNLOADMANAGER_FILEDOWNLOAD_H

#include <QList>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetHashesResult.h>

#include <Protos/common.pb.h>

#include <priv/OccupiedPeers.h>
#include <priv/Download.h>
#include <priv/ChunkDownload.h>

namespace DM
{
   class DownloadManager;
   class FileDownload : public Download
   {
      Q_OBJECT
   public:
      FileDownload(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         OccupiedPeers& occupiedPeersAskingForHashes,
         OccupiedPeers& occupiedPeersDownloadingChunk,
         Common::Hash peerSourceID,
         const Protos::Common::Entry& entry
      );

      int getDownloadRate() const;
      int getProgress() const;

      QSharedPointer<ChunkDownload> getAChunkToDownload();

   public slots:
      bool retreiveHashes();

   signals:
      void changeOccurs();

   protected slots:
      void retrievePeer();

   private slots:
      void result(const Protos::Core::GetHashesResult& result);
      void nextHash(const Common::Hash& hash);

      void downloadStarted();
      void downloadFinished();

   private:
      void connectChunkDownloadSignals(QSharedPointer<ChunkDownload> chunkDownload);

      const int NB_CHUNK;
      DownloadManager* downloadManager;

      QList< QSharedPointer<FM::IChunk> > chunksWithoutDownload;
      QList< QSharedPointer<ChunkDownload> > chunkDownloads;

      OccupiedPeers& occupiedPeersAskingForHashes;
      OccupiedPeers& occupiedPeersDownloadingChunk;

      int nbHashesKnown;
      QSharedPointer<PM::IGetHashesResult> getHashesResult;

      bool fileCreated;

      QTimer timer; // Used to periodically try to retrieve hashes.
   };
}
#endif
