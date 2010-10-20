#include <priv/RemoteControlManager.h>
using namespace RCM;

#include <Common/Settings.h>

#include <priv/Log.h>

RemoteControlManager::RemoteControlManager(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   QSharedPointer<NL::INetworkListener> networkListener
) :
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   networkListener(networkListener)
{
   this->tcpServer.listen(QHostAddress::Any, SETTINGS.get<quint32>("remote_control_port"));
   connect(&this->tcpServer, SIGNAL(newConnection()), this, SLOT(newConnection()));

   L_DEBU(QString("Listen new remoteConnection on port %1").arg(SETTINGS.get<quint32>("remote_control_port")));
}

void RemoteControlManager::newConnection()
{
   QTcpSocket* socket = this->tcpServer.nextPendingConnection();

   if (this->connections.size() > SETTINGS.get<quint32>("remote_max_nb_connection"))
   {
      L_WARN("Cannot handle new connection, too many connection");
      delete socket;
      return;
   }

   RemoteConnection* remoteConnection = new RemoteConnection(
      this->fileManager,
      this->peerManager,
      this->uploadManager,
      this->downloadManager,
      this->networkListener,
      socket
   );

   connect(remoteConnection, SIGNAL(deleted(RemoteConnection*)), this, SLOT(connectionDeleted(RemoteConnection*)));
   connections << remoteConnection;
}

void RemoteControlManager::connectionDeleted(RemoteConnection* connection)
{
   this->connections.removeOne(connection);
}
