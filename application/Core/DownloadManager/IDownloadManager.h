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
        * @param entry It must have the field 'shared_dir' and 'shared_dir.shared_name' should be set.
        */
      virtual void addDownload(const Protos::Common::Entry& entry, Common::Hash peerSource) = 0;

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
