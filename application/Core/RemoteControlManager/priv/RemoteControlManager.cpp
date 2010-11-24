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
   const quint32 PORT = SETTINGS.get<quint32>("remote_control_port");

   if (!this->tcpServer.listen(QHostAddress::Any, PORT))
      if (!this->tcpServer.listen(QHostAddress::AnyIPv6, PORT))
         L_ERRO(QString("Unable to listen on port %1").arg(PORT));

   connect(&this->tcpServer, SIGNAL(newConnection()), this, SLOT(newConnection()));

   L_DEBU(QString("Listen new remoteConnection on port %1").arg(PORT));
}

RemoteControlManager::~RemoteControlManager()
{
   for (QListIterator<RemoteConnection*> i(this->connections); i.hasNext();)
   {
      RemoteConnection* connection = i.next();
      disconnect(connection, SIGNAL(deleted(RemoteConnection*)), this, SLOT(connectionDeleted(RemoteConnection*)));
      delete connection;
   }

   L_DEBU("RemoteControlManager deleted");
}

void RemoteControlManager::newConnection()
{
   QTcpSocket* socket = this->tcpServer.nextPendingConnection();

   if (static_cast<quint32>(this->connections.size()) > SETTINGS.get<quint32>("remote_max_nb_connection"))
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
