#ifndef DOWNLOADMANAGER_IDOWNLOADMANAGER_H
#define DOWNLOADMANAGER_IDOWNLOADMANAGER_H

#include <QList>
#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

#include <Core/DownloadManager/IChunkDownload.h>

namespace DM
{
   class IDownload;

   class IDownloadManager
   {
   public:
      virtual ~IDownloadManager() {}

      /**
        * @param entry It must have the field 'shared_dir'.
        */
      virtual void addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry) = 0;

      /**
        * @remarks The returned download pointers must not be retained.
        */
      virtual QList<IDownload*> getDownloads() = 0;

      virtual QList< QSharedPointer<IChunkDownload> > getUnfinishedChunks(int n) = 0;

      /**
        * @return Byte/s.
        */
      virtual int getDownloadRate() const = 0;
   };
}
#endif
