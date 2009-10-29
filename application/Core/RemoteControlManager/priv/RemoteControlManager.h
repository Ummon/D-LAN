#ifndef REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H
#define REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H

#include <IRemoteControlManager.h>

namespace UM { class IUploadManager; }
namespace DM { class IDownloadManager; }
namespace PM { class IPeerManager; }
namespace FM { class IFileManager; }
namespace NL { class INetworkListener; }

namespace RCM
{
   class RemoteConnection;

   class RemoteControlManager : public IRemoteControlManager
   {
   private:
      RemoteConnection* remoteConnection;
      UM::IUploadManager* uploadManager;
      DM::IDownloadManager* downloadManager;
      PM::IPeerManager* peerManager;
      FM::IFileManager* fileManager;
      NL::INetworkListener* networkListener;
   };
}
#endif
