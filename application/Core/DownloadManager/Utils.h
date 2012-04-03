#ifndef DOWNLOADMANAGER_UTILS_H
#define DOWNLOADMANAGER_UTILS_H

#include <QString>

#include <Core/DownloadManager/IDownload.h>

namespace DM
{
   class Utils
   {
   public:
#ifdef DEBUG
      static QString getStatusStr(Status status);
#endif
   };
}

#endif
