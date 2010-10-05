#ifndef DOWNLOADMANAGER_CHUNKDOWNLOADER_H
#define DOWNLOADMANAGER_CHUNKDOWNLOADER_H

#include <QThread>

#include <Common/Hash.h>

namespace DM
{
   class ChunkDownload;
   class DownloadManager;

   class ChunkDownloader : public QThread
   {
      void run();
      Common::Hash getHash();
      void setPeerIDs(const QList<Common::Hash>& peerIDs);

   private:
      ChunkDownload* chunkDownload;
      DownloadManager* downloadManager;

      bool finished; ///< When true then the associated chunk is entirely downloaded.
   };
}
#endif
