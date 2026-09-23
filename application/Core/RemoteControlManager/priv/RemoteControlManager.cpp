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

#include <algorithm>

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/Network/RemoteControlTls.h>

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
   try
   {
      this->tlsConfiguration = Common::RemoteControlTls::serverConfiguration();
      L_USER(QString("Remote-control TLS certificate SHA-256: %1")
         .arg(Common::RemoteControlTls::fingerprint(this->tlsConfiguration.localCertificate())));
   }
   catch (const QString& error)
   {
      L_ERRO(QString("Remote TLS access disabled: %1. Local access remains available.").arg(error));
   }
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
   for (RemoteConnection* connection : std::as_const(this->connections))
   {
      connection->disconnect(this);
      delete connection;
   }

   L_DEBU("RemoteControlManager deleted");
}

void RemoteControlManager::newConnection()
{
   auto* socket = static_cast<QSslSocket*>(static_cast<QTcpServer*>(this->sender())->nextPendingConnection());

   if (!socket)
      return;

   const bool local = Common::Global::isLocal(socket->peerAddress());
   if (!local && this->tlsConfiguration.isNull())
   {
      socket->abort();
      socket->deleteLater();
      return;
   }

   if (socket->state() != QAbstractSocket::ConnectedState)
   {
      L_DEBU("New connection already closed, it is discarded");
      socket->deleteLater();
      return;
   }

   // A connection which has just been closed can still be in the list, it is waiting to be deleted,
   // see 'RemoteConnection::onDisconnected()'. Such a connection doesn't take a slot anymore.
   const auto nbCurrentConnections = std::count_if(this->connections.cbegin(), this->connections.cend(),
      [](const RemoteConnection* connection) { return connection->isConnected(); });

   if (static_cast<quint64>(nbCurrentConnections) >= SETTINGS.get<quint32>("remote_max_nb_connection"))
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
      socket,
      local
   );

   connect(remoteConnection, &RemoteConnection::deleted, this, &RemoteControlManager::connectionDeleted, Qt::DirectConnection);
   connect(remoteConnection, &RemoteConnection::languageDefined, this, &RemoteControlManager::languageDefined);
   this->connections << remoteConnection;
   if (local)
      remoteConnection->startListening();
   else
   {
      // Count connections during TLS too, and bound clients that never finish
      // the handshake. No protocol messages are sent before encrypted().
      auto* timeout = new QTimer(remoteConnection);
      timeout->setSingleShot(true);
      connect(timeout, &QTimer::timeout, socket, &QSslSocket::abort);
      connect(socket, &QSslSocket::encrypted, remoteConnection, [remoteConnection, timeout] {
         timeout->stop();
         remoteConnection->startListening();
      });
      connect(socket, &QSslSocket::errorOccurred, remoteConnection, [socket](QAbstractSocket::SocketError) {
         // Once encrypted, an error is the end of a normal session (e.g. the GUI closing), not a TLS failure.
         if (!socket->isEncrypted())
            L_WARN(QString("Remote TLS connection failed: %1").arg(socket->errorString()));
      });
      socket->setSslConfiguration(this->tlsConfiguration);
      timeout->start(TLS_HANDSHAKE_TIMEOUT);
      socket->startServerEncryption();
   }
}

void RemoteControlManager::connectionDeleted(RemoteConnection* connection)
{
   this->connections.removeOne(connection);
}
