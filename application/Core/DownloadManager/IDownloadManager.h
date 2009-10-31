#ifndef DOWNLOADMANAGER_IDOWNLOADMANAGER_H
#define DOWNLOADMANAGER_IDOWNLOADMANAGER_H

#include <QList>
#include <QSharedPointer>

#include <IChunkDownload.h>

namespace DM
{
   class IDownload;

   class IDownloadManager
   {
   public:
      virtual ~IDownloadManager() {}

      virtual QList<IDownload*> getDownloads() = 0;
      virtual QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n) = 0;
   };
}
#endif
