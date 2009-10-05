#ifndef DOWNLOADMANAGER_ICHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_ICHUNKDOWNLOAD_H

#include <QtGlobal>

#include <Common/Hash.h>

namespace DownloadManager
{
   class IChunkDownload
   {
   public:
      virtual Common::Hash getHash() = 0;
      virtual void setPeerIDs(const QList<quint32>& peerIDs) = 0;
   };
}
#endif
