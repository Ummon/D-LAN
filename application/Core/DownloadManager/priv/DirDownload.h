#ifndef DOWNLOADMANAGER_DIRDOWNLOAD_H
#define DOWNLOADMANAGER_DIRDOWNLOAD_H

#include <Protos/common.pb.h>

#include <priv/Download.h>

namespace DM
{
   class DirDownload : public Download
   {
   public:
      virtual ~DirDownload();
   private:
      Protos::Common::DirEntry remoteEntry;
   };
}
#endif
