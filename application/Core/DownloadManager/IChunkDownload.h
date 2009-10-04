#ifndef DOWNLOADMANAGER_ICHUNKDOWNLOAD_H
#define DOWNLOADMANAGER_ICHUNKDOWNLOAD_H

#include <QtGlobal>

#include <Common/Hash.h>
using namespace Common;

namespace DownloadManager
{   
   class IChunkDownload
   {
   public:
      virtual Hash getHash() = 0;
      virtual void setPeerIDs(const QList<quint32>& peerIDs) = 0;
   };
}
#endif
