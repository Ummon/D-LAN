#ifndef DOWNLOADMANAGER_IDOWNLOADMANAGER_H
#define DOWNLOADMANAGER_IDOWNLOADMANAGER_H

#include <QList>
#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

#include <IChunkDownload.h>

namespace DM
{
   class IDownload;

   class IDownloadManager : public QObject
   {
   public:
      virtual ~IDownloadManager() {}

      virtual void addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry) = 0;
      virtual QList<IDownload*> getDownloads() = 0;
      virtual QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n) = 0;
   };
}
#endif
