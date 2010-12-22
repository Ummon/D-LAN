/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
      disconnect(connection, SIGNAL(chatMessageSent(const QString&)), this, SLOT(chatMessageSent(const QString&)));
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

   connect(remoteConnection, SIGNAL(deleted(RemoteConnection*)), this, SLOT(connectionDeleted(RemoteConnection*)), Qt::DirectConnection);
   connect(remoteConnection, SIGNAL(chatMessageSent(const QString&)), this, SLOT(chatMessageSent(const QString&)), Qt::DirectConnection);
   connections << remoteConnection;
}

void RemoteControlManager::connectionDeleted(RemoteConnection* connection)
{
   this->connections.removeOne(connection);
}

/**
  * When a message is sent to the other peers, it must also be forwarded to the other remote connections.
  */
void RemoteControlManager::chatMessageSent(const QString& message)
{
   RemoteConnection* sender = static_cast<RemoteConnection*>(this->sender());
   for (QListIterator<RemoteConnection*> i(this->connections); i.hasNext();)
   {
      RemoteConnection* remoteConnection = i.next();
      if (remoteConnection != sender)
         remoteConnection->sendMessageToItself(message);
   }
}
