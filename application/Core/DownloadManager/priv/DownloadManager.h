#ifndef DOWNLOADMANAGER_DOWNLOADMANAGER_H
#define DOWNLOADMANAGER_DOWNLOADMANAGER_H

#include <QLinkedList>
#include <QList>
#include <QSet>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IDownloadManager.h>

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
      QList<IDownload*> getDownloads();
      QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n);

   private:
      void addNewDownload(FileDownload* download);

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      QLinkedList<Download*> downloads;

      QSet<Common::Hash> peerIDAskingHashes;

      QList<ChunkDownloader*> chunkDownloaders;
   };
}
#endif
