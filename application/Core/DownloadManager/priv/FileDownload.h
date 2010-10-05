#ifndef DOWNLOADMANAGER_FILEDOWNLOAD_H
#define DOWNLOADMANAGER_FILEDOWNLOAD_H

#include <QList>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <Protos/common.pb.h>

#include <priv/Download.h>

namespace DM
{
   class ChunkDownload;
   class FileDownload : public Download
   {
   public:
      FileDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

   private:
      QList<ChunkDownload*> chunkDownloads;
      //FileManager::IFile* file;
   };
}
#endif
