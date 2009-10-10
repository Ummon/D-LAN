#ifndef CORE_CORE_H
#define CORE_CORE_H

/*
#include <RemoteControlManager/IRemoteControlManager.h>
#include <PeerManager/IPeerManager.h>
#include <DownloadManager/IDownloadManager.h>
#include <UploadManager/IUploadManager.h>*/

#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>
#include <FileManager/IFileManager.h>
#include <NetworkListener/INetworkListener.h>

namespace LogManager { class ILogger; }
namespace RemoteControlManager { class IRemoteControlManager; }
namespace PeerManager { class IPeerManager; }
namespace NetworkListener { class INetworkListener; }
namespace DownloadManager { class IDownloadManager; }
namespace UploadManager { class IUploadManager; }

namespace Core
{
   class Core
   {
   public:
      Core();

   private:
      QSharedPointer<LogManager::ILogger> logger;
      QSharedPointer<FileManager::IFileManager> fileManager;
      QSharedPointer<NetworkListener::INetworkListener> networkListener;

      RemoteControlManager::IRemoteControlManager* remoteControlManager;
      PeerManager::IPeerManager* peerManager;
      DownloadManager::IDownloadManager* downloadManager;
      UploadManager::IUploadManager* uploadManager;
   };
}
#endif
