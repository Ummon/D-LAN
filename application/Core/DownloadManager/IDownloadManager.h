#ifndef DOWNLOADMANAGER_IDOWNLOADMANAGER_H
#define DOWNLOADMANAGER_IDOWNLOADMANAGER_H

#include <QList>

namespace DownloadManager
{  
   class IDownload;
   class IChunkDownload;
   
   class IDownloadManager
   {
   public:
      virtual QList<IDownload*> getDownloads() = 0;
      virtual QList<IChunkDownload*> getUnfinishedChunks(int n) = 0;  
   };
}
#endif
