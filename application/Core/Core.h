#ifndef CORE_CORE_H
#define CORE_CORE_H

#include <QSharedPointer>

#include <FileManager/IFileManager.h>
#include <PeerManager/IPeerManager.h>
#include <UploadManager/IUploadManager.h>
#include <DownloadManager/IDownloadManager.h>
#include <NetworkListener/INetworkListener.h>

namespace Core // Best than the Arm.
{
   class Core
   {
   public:
      Core();
      virtual ~Core();

   private:
      void checkSettingsIntegrity();

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener;
   };
}
#endif
