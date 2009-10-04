#ifndef DOWNLOADMANAGER_FILEDOWNLOAD_H
#define DOWNLOADMANAGER_FILEDOWNLOAD_H

#include <QList>

#include <Protos/common.pb.h>

#include <priv/Download.h>

namespace FileManager { class IFile; } 

namespace DownloadManager
{
   class ChunkDownload;
   class FileDownload : public Download
   {
   public:
      virtual ~FileDownload();
   
   private:
      QList<ChunkDownload*> chunkDownloads;
      FileManager::IFile* file;
      Protos::Common::FileEntry remoteEntry;
   };
}
#endif
