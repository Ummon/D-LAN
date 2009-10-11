#ifndef CORE_CORE_H
#define CORE_CORE_H

/*
#include <RemoteControlManager/IRemoteControlManager.h>

#include <DownloadManager/IDownloadManager.h>
#include <UploadManager/IUploadManager.h>*/

#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>
#include <FileManager/IFileManager.h>
#include <NetworkListener/INetworkListener.h>
#include <NetworkListener/IChat.h>
#include <PeerManager/IPeerManager.h>

#include <QObject>


namespace LogManager { class ILogger; }
namespace RemoteControlManager { class IRemoteControlManager; }
namespace PeerManager { class IPeerManager; }
namespace NetworkListener { class INetworkListener; }
namespace DownloadManager { class IDownloadManager; }
namespace UploadManager { class IUploadManager; }

namespace Core
{
   class Core : public QObject
 {
     Q_OBJECT

   public:
      Core();
      virtual ~Core() {}

   private:
      QSharedPointer<LogManager::ILogger> logger;
      QSharedPointer<FileManager::IFileManager> fileManager;
      QSharedPointer<NetworkListener::INetworkListener> networkListener;
      QSharedPointer<PeerManager::IPeerManager> peerManager;

      RemoteControlManager::IRemoteControlManager* remoteControlManager;
      DownloadManager::IDownloadManager* downloadManager;
      UploadManager::IUploadManager* uploadManager;

   public slots:
      void dBug_chat(const Protos::Core::ChatMessage& message);

   };
}
#endif
