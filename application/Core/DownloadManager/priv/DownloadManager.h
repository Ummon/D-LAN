#ifndef DOWNLOADMANAGER_DOWNLOADMANAGER_H
#define DOWNLOADMANAGER_DOWNLOADMANAGER_H

#include <QSharedPointer>

#include <IDownloadManager.h>

namespace DownloadManager
{
   class Download;
   class ChunkDownloader;

   class DownloadManager : public IDownloadManager
   {
   public:
      virtual ~DownloadManager();
      QList<IDownload*> getDownloads();
      QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n);

   private:
      QList<Download*> downloads;
      QList<ChunkDownloader*> chunkDownloaders;
   };
}
#endif
