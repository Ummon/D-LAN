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
  
#include <priv/PeerManager.h>
using namespace PM;

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/PersistentData.h>
#include <Common/Settings.h>
#include <Common/Network/MessageHeader.h>

#include <priv/Constants.h>

/**
  * @class PM::PeerManager
  *
  */

LOG_INIT_CPP(PeerManager)

PeerManager::PeerManager(QSharedPointer<FM::IFileManager> fileManager) :
   fileManager(fileManager), self(new PeerSelf(this, this->fileManager))
{
   this->timer.setInterval(SETTINGS.get<quint32>("pending_socket_timeout") / 10);
   connect(&this->timer, &QTimer::timeout, this, &PeerManager::checkIdlePendingSockets);
}

PeerManager::~PeerManager()
{
   for (QMapIterator<Common::Hash, Peer*> i(this->peers); i.hasNext();)
      delete i.next().value();
   delete this->self;

   L_DEBU("PeerManager deleted");
}

/**
  * Set the current nick.
  */
void PeerManager::setNick(const QString& nick)
{
   this->self->setNick(nick);
}

IPeer* PeerManager::getSelf()
{
   return this->self;
}

int PeerManager::getNbOfPeers() const
{
   int n = 0;
   for (QMapIterator<Common::Hash, Peer*> i(this->peers); i.hasNext();)
      if (i.next().value()->isAlive())
         n++;
   return n;
}

QList<IPeer*> PeerManager::getPeers() const
{
   QList<IPeer*> peers;

   for (QMapIterator<Common::Hash, Peer*> i(this->peers); i.hasNext();)
   {
      Peer* peer = i.next().value();
      if (peer->isAlive())
         peers << peer;
   }

   return peers;
}

IPeer* PeerManager::getPeer(const Common::Hash& ID)
{
   if (ID.isNull())
      return nullptr;

   if (this->self->getID() == ID)
      return this->self;

   auto it = this->peers.find(ID);
   if (it != this->peers.end())
      return *it;

   return nullptr;
}

IPeer* PeerManager::createPeer(const Common::Hash& ID, const QString& nick)
{
   IPeer* existingPeer = this->getPeer(ID);
   if (existingPeer)
      return existingPeer;

   Peer* peer = new Peer(this, this->fileManager, ID, nick);
   connect(peer, &Peer::unblocked, this, &PeerManager::peerUnblocked);
   this->peers.insert(peer->getID(), peer);

   return peer;
}

/**
  * A peer just send a IAmAlive packet, we update information about it
  */
void PeerManager::updatePeer(
   const Common::Hash& ID,
   const QHostAddress& IP,
   quint16 port,
   const QString& nick,
   const quint64& sharingAmount,
   const QString& coreVersion,
   quint32 downloadRate,
   quint32 uploadRate,
   quint32 protocolVersion
)
{
   if (ID.isNull() || ID == this->self->getID())
      return;

   L_DEBU(QString("%1 (%2) is alive!").arg(ID.toStr()).arg(nick));

   Peer* peer = static_cast<Peer*>(this->getPeer(ID));
   if (!peer)
   {
      peer = new Peer(this, this->fileManager, ID);
      connect(peer, &Peer::unblocked, this, &PeerManager::peerUnblocked);
      this->peers.insert(peer->getID(), peer);
   }

   const bool wasDead = !peer->isAlive();

   peer->update(IP, port, nick, sharingAmount, coreVersion, downloadRate, uploadRate, protocolVersion);

   if (wasDead && peer->isAvailable())
      emit peerBecomesAvailable(peer);
}

void PeerManager::removePeer(const Common::Hash& ID, const QHostAddress& IP)
{
   if (ID.isNull() || ID == this->self->getID())
      return;

   Peer* peer = static_cast<Peer*>(this->getPeer(ID));
   if (peer && IP == peer->getIP())
      peer->setAsDead();
}

void PeerManager::removeAllPeers()
{
   for (QMapIterator<Common::Hash, Peer*> i(this->peers); i.hasNext();)
      i.next().value()->setAsDead();
}

void PeerManager::newConnection(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      return;

   // Detach the socket to use it into a thread.
   tcpSocket->setParent(0);

   if (!tcpSocket->isValid())
   {
      L_DEBU("PeerManager::newConnection(..): socket isn't valid");
      tcpSocket->deleteLater();
   }
   else
   {
      L_DEBU(QString("New pending socket from %1").arg(tcpSocket->peerAddress().toString()));

      if (!this->timer.isActive())
         this->timer.start();

      connect(tcpSocket, SIGNAL(readyRead()), this, SLOT(dataReceived()), Qt::DirectConnection);
      connect(tcpSocket, SIGNAL(disconnected()), this, SLOT(disconnected()), Qt::DirectConnection);
      this->pendingSockets << PendingSocket(tcpSocket);
      this->dataReceived(tcpSocket); // The case where some data arrived before the 'connect' above.
   }
}

void PeerManager::onGetChunk(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PeerMessageSocket> socket)
{
   if (this->receivers(SIGNAL(getChunk(QSharedPointer<FM::IChunk>, int, QSharedPointer<PM::ISocket>))) < 1)
   {
      Protos::Core::GetChunkResult mess;
      mess.set_status(Protos::Core::GetChunkResult::ERROR_UNKNOWN);
      socket->send(Common::MessageHeader::CORE_GET_CHUNK_RESULT, mess);
      socket->finished();
      L_ERRO("PeerManager::onGetChunk(..) : no slot connected to the signal 'getChunk(..)'");
      return;
   }

   emit getChunk(chunk, offset, socket);
}

void PeerManager::dataReceived(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      tcpSocket = static_cast<QTcpSocket*>(this->sender());

   if (tcpSocket->bytesAvailable() >= Common::MessageHeader::HEADER_SIZE)
   {
      const Common::MessageHeader header = Common::MessageHeader::readHeader(*tcpSocket, false);
      Peer* p = static_cast<Peer*>(this->getPeer(header.getSenderID()));

      this->removeFromPending(tcpSocket);

      if (p)
         p->newConnexion(tcpSocket);
      else
      {
         L_DEBU(QString("PeerManager::dataReceived(..): No peer. Header: %1").arg(header.toStr()));
         this->disconnected(tcpSocket);
      }
   }
}

void PeerManager::disconnected(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      tcpSocket = static_cast<QTcpSocket*>(this->sender());

   L_DEBU("Pending socket disconnected");

   this->removeFromPending(tcpSocket);
   tcpSocket->deleteLater();
}

void PeerManager::checkIdlePendingSockets()
{
   for (QMutableListIterator<PendingSocket> i(this->pendingSockets); i.hasNext();)
   {
      PendingSocket& pendingSocket = i.next();
      static const int SOCKET_TIMEOUT = SETTINGS.get<quint32>("pending_socket_timeout");
      if (pendingSocket.t.elapsed() > SOCKET_TIMEOUT)
      {
         L_DEBU("Pending socket timed out -> removed");
         pendingSocket.socket->disconnect();
         pendingSocket.socket->deleteLater();
         i.remove();
      }
   }

   if (this->pendingSockets.isEmpty())
      this->timer.stop();
}

void PeerManager::peerUnblocked()
{
   Peer* peer = static_cast<Peer*>(this->sender());
   if (peer->isAvailable())
      emit peerBecomesAvailable(peer);
}

void PeerManager::removeFromPending(QTcpSocket* socket)
{
   for (QMutableListIterator<PendingSocket> i(this->pendingSockets); i.hasNext();)
   {
      PendingSocket& pendingSocket = i.next();
      if (pendingSocket.socket == socket)
      {
         pendingSocket.socket->disconnect();
         i.remove();
         break;
      }
   }

   if (this->pendingSockets.isEmpty())
      this->timer.stop();
}
