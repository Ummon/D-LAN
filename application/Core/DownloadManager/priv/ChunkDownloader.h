#ifndef DOWNLOADMANAGER_CHUNKDOWNLOADER_H
#define DOWNLOADMANAGER_CHUNKDOWNLOADER_H

#include <QThread>
#include <QSharedPointer>

#include <Common/Hash.h>

#include <priv/ChunkDownload.h>

namespace DM
{
   class DownloadManager;

   class ChunkDownloader : public QThread
   {
      Q_OBJECT
   public:
      ChunkDownloader();

      void run();

      bool isIdle() const;
      Common::Hash getHash();

      void setPeerIDs(const QList<Common::Hash>& peerIDs);

//   signals:
//      /**
//        * Sent when the downloader
//        */
//      void idle(ChunkDownloader* downloader);

//   public slots:
//      void chunkReadyToDownload(QSharedPointer<ChunkDownload> chunkDownload);

   private:
      QSharedPointer<ChunkDownload> chunkDownload;

      bool idle;

      //bool finished; ///< When true the associated chunk is entirely downloaded.
   };
}
#endif
