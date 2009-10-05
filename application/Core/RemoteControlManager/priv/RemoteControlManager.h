#ifndef REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H
#define REMOTECONTROLMANAGER_REMOTECONTROLMANAGER_H

#include <IRemoteControlManager.h>

namespace UploadManager { class IUploadManager; }
namespace DownloadManager { class IDownloadManager; }
namespace PeerManager { class IPeerManager; }
namespace FileManager { class IFileManager; }
namespace NetworkListener { class INetworkListener; }

namespace RemoteControlManager
{
   class RemoteConnection;

   class RemoteControlManager : public IRemoteControlManager
   {
   private:
      RemoteConnection* remoteConnection;
      UploadManager::IUploadManager* uploadManager;
      DownloadManager::IDownloadManager* downloadManager;
      PeerManager::IPeerManager* peerManager;
      FileManager::IFileManager* fileManager;
      NetworkListener::INetworkListener* networkListener;
   };
}
#endif
