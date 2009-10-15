#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QSharedPointer>

#include <INetworkListener.h>

#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>
#include <FileManager/IFileManager.h>
#include <NetworkListener/INetworkListener.h>
#include <NetworkListener/IChat.h>
#include <PeerManager/IPeerManager.h>

using namespace NetworkListener;

namespace LogManager { class ILogger; }
namespace RemoteControlManager { class IRemoteControlManager; }
namespace PeerManager { class IPeerManager; }
namespace NetworkListener { class INetworkListener; }
namespace DownloadManager { class IDownloadManager; }
namespace UploadManager { class IUploadManager; }

class Tests : public QObject
{
Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void testSending();
   void testReception();
   void messageRecevied(const Protos::Core::ChatMessage& message);


private :
   QSharedPointer<LogManager::ILogger> logger;
   QSharedPointer<FileManager::IFileManager> fileManager;
   QSharedPointer<NetworkListener::INetworkListener> networkListener;
   QSharedPointer<PeerManager::IPeerManager> peerManager;
   bool isMessageRecevied;
};

#endif
