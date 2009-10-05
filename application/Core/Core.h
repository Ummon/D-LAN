#ifndef CORE_CORE_H
#define CORE_CORE_H

/*
#include <RemoteControlManager/IRemoteControlManager.h>
#include <FileManager/IFileManager.h>
#include <PeerManager/IPeerManager.h>
#include <NetworkListener/INetworkListener.h>
#include <DownloadManager/IDownloadManager.h>
#include <UploadManager/IUploadManager.h>*/

#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>

namespace LogManager { class ILogger; }
namespace RemoteControlManager { class IRemoteControlManager; }
namespace FileManager { class IFileManager; }
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

      RemoteControlManager::IRemoteControlManager* remoteControlManager;
      FileManager::IFileManager* fileManager;
      PeerManager::IPeerManager* peerManager;
      NetworkListener::INetworkListener* networkListener;
      DownloadManager::IDownloadManager* downloadManager;
      UploadManager::IUploadManager* uploadManager;
   };
}
#endif
