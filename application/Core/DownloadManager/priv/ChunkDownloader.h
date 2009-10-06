#ifndef DOWNLOADMANAGER_CHUNKDOWNLOADER_H
#define DOWNLOADMANAGER_CHUNKDOWNLOADER_H

#include <QThread>

namespace DownloadManager
{
   class ChunkDownload;
   class DownloadManager;

   class ChunkDownloader : public QThread
   {
   private:
      ChunkDownload* chunkDownload;
      DownloadManager* downloadManager;

      bool finished; ///< When true then the associated chunk is entirely downloaded.
   };
}
#endif
