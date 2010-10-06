#ifndef DOWNLOADMANAGER_FILEDOWNLOAD_H
#define DOWNLOADMANAGER_FILEDOWNLOAD_H

#include <QList>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetHashesResult.h>

#include <Protos/common.pb.h>

#include <priv/Download.h>
#include <priv/ChunkDownload.h>

namespace DM
{
   class FileDownload : public Download
   {
      Q_OBJECT
   public:
      FileDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

      void retreiveHashes();

   signals:
      /**
        * When a chunkDownload know its hash and has at least on IPeer associated this signal is emitted.
        */
      void chunkReadyToDownload(QSharedPointer<ChunkDownload> chunkDownload);

   private slots:
      void chunkReadyToDownload(ChunkDownload*);

      void result(const Protos::Core::GetHashesResult& result);
      void nextHash(const Common::Hash& hash);

   private:
      int nbHashes;
      QSharedPointer<PM::IGetHashesResult> getHashesResult;
      QList< QSharedPointer<ChunkDownload> > chunkDownloads;
      QList< QSharedPointer<FM::IChunk> > chunksWithoutDownload;
   };
}
#endif
