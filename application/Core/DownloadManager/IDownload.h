#ifndef DOWNLOADMANAGER_IDOWNLOAD_H
#define DOWNLOADMANAGER_IDOWNLOAD_H

#include <QtGlobal>

namespace DownloadManager
{
   enum Status
   {
     Downloading,
     Complete,
     NoSource,
     NotFound,
     UnkownPeer,
     Initializing,
     Queued
   };
   
   class IDownload
   {
   public:
      virtual quint32 getId() = 0;
      virtual Status getStatus() = 0;
      virtual bool isDir() = 0;
      virtual quint32 getProgress() = 0;
   };
}
#endif
