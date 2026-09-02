/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <priv/RemoteControlManager.h>
using namespace RCM;

#include <Common/Settings.h>

LOG_INIT_CPP(RemoteControlManager)

RemoteControlManager::RemoteControlManager(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   QSharedPointer<NL::INetworkListener> networkListener,
   QSharedPointer<CS::IChatSystem> chatSystem
) :
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   networkListener(networkListener),
   chatSystem(chatSystem)
{
   const quint32 PORT = SETTINGS.get<quint32>("remote_control_port");

   const bool okIPv4 = this->tcpServerIPv4.listen(QHostAddress::AnyIPv4, PORT);
   const bool okIPv6 = this->tcpServerIPv6.listen(QHostAddress::AnyIPv6, PORT);

   connect(&this->tcpServerIPv4, &QTcpServer::newConnection, this, &RemoteControlManager::newConnection);
   connect(&this->tcpServerIPv6, &QTcpServer::newConnection, this, &RemoteControlManager::newConnection);

   if (!okIPv4)
      L_WARN(QString("Unable to listen on port %1 (IPv4): %2").arg(PORT).arg(this->tcpServerIPv4.errorString()));

   if (!okIPv6)
      L_WARN(QString("Unable to listen on port %1 (IPv6): %2").arg(PORT).arg(this->tcpServerIPv6.errorString()));

   if (!okIPv4 && !okIPv6)
      L_ERRO(QString("Unable to listen on port %1, no remote control will be possible").arg(PORT));
   else
      L_DEBU(QString("Listen new remoteConnection on port %1").arg(PORT));
}

RemoteControlManager::~RemoteControlManager()
{
   for (QListIterator<RemoteConnection*> i(this->connections); i.hasNext();)
   {
      RemoteConnection* connection = i.next();
      connection->disconnect(this);
      delete connection;
   }

   L_DEBU("RemoteControlManager deleted");
}

void RemoteControlManager::newConnection()
{
   QTcpSocket* socket = static_cast<QTcpServer*>(this->sender())->nextPendingConnection();

   if (!socket)
      return;

   if (socket->state() != QAbstractSocket::ConnectedState)
   {
      L_DEBU("New connection already closed, it is discarded");
      socket->deleteLater();
      return;
   }

   // A connection which has just been closed can still be in the list, it is waiting to be deleted,
   // see 'RemoteConnection::onDisconnected()'. Such a connection doesn't take a slot anymore.
   quint32 nbCurrentConnections = 0;
   for (QListIterator<RemoteConnection*> i(this->connections); i.hasNext();)
      if (i.next()->isConnected())
         nbCurrentConnections++;

   if (nbCurrentConnections >= SETTINGS.get<quint32>("remote_max_nb_connection"))
   {
      L_WARN("Cannot handle new connection, too many connection");
      socket->close();
      socket->deleteLater();
      return;
   }

   RemoteConnection* remoteConnection = new RemoteConnection(
      this->fileManager,
      this->peerManager,
      this->uploadManager,
      this->downloadManager,
      this->networkListener,
      this->chatSystem,
      socket
   );

   connect(remoteConnection, &RemoteConnection::deleted, this, &RemoteControlManager::connectionDeleted, Qt::DirectConnection);
   connect(remoteConnection, &RemoteConnection::languageDefined, this, &RemoteControlManager::languageDefined);
   this->connections << remoteConnection;
}

void RemoteControlManager::connectionDeleted(RemoteConnection* connection)
{
   this->connections.removeOne(connection);
}
