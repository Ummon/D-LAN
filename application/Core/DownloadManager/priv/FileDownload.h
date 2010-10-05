#ifndef DOWNLOADMANAGER_FILEDOWNLOAD_H
#define DOWNLOADMANAGER_FILEDOWNLOAD_H

#include <QList>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <Protos/common.pb.h>

#include <priv/Download.h>
#include <priv/ChunkDownload.h>

namespace DM
{
   class ChunkDownload;
   class FileDownload : public Download
   {
      Q_OBJECT
   public:
      FileDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

      void retreiveHashes();

   private slots:
      void result(const Protos::Core::GetHashesResult& result);
      void nextHash(const Common::Hash& hash);

   private:
      QList<ChunkDownload*> chunkDownloads;
   };
}
#endif
