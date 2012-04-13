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

LOG_INIT_CPP(PeerManager);

PeerManager::PeerManager(QSharedPointer<FM::IFileManager> fileManager) :
   fileManager(fileManager), self(new PeerSelf(this, this->fileManager))
{
   this->timer.setInterval(SETTINGS.get<quint32>("pending_socket_timeout") / 10);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(checkIdlePendingSockets()));
}

PeerManager::~PeerManager()
{
   for (QListIterator<Peer*> i(this->peers); i.hasNext();)
      delete i.next();
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

QList<IPeer*> PeerManager::getPeers()
{
   QList<IPeer*> peers;

   for (QListIterator<Peer*> i(this->peers); i.hasNext();)
   {
      Peer* peer = i.next();
      if (peer->isAlive())
         peers << peer;
   }

   return peers;
}

IPeer* PeerManager::getPeer(const Common::Hash& ID)
{
   return this->getPeer_(ID);
}

Peer* PeerManager::getPeer_(const Common::Hash& ID)
{
   if (ID.isNull())
      return 0;

   if (this->self->getID() == ID)
      return this->self;

   for (QListIterator<Peer*> i(this->peers); i.hasNext();)
   {
      Peer* peer = i.next();
      if (peer->getID() == ID)
         return peer;
   }

   return 0;
}

IPeer* PeerManager::createPeer(const Hash& ID, const QString& nick)
{
   IPeer* existingPeer = this->getPeer(ID);
   if (existingPeer)
      return existingPeer;

   Peer* peer = new Peer(this, this->fileManager, ID, nick);
   connect(peer, SIGNAL(unbanned()), this, SLOT(peerUnbanned()));
   this->peers << peer;

   return peer;
}

/**
  * A peer just send a IAmAlive packet, we update information about it
  */
void PeerManager::updatePeer(const Common::Hash& ID, const QHostAddress& IP, quint16 port, const QString& nick, const quint64& sharingAmount, const QString& coreVersion)
{
   if (ID.isNull() || ID == this->self->getID())
      return;

   L_DEBU(QString("%1 (%2) is alive!").arg(ID.toStr()).arg(nick));

   Peer* peer = this->getPeer_(ID);
   if (!peer)
   {
      peer = new Peer(this, this->fileManager, ID);
      connect(peer, SIGNAL(unbanned()), this, SLOT(peerUnbanned()));
      this->peers << peer;
   }

   const bool wasDead = !peer->isAlive();

   peer->update(IP, port, nick, sharingAmount, coreVersion);

   if (wasDead && peer->isAvailable())
      emit peerBecomesAvailable(peer);
}

void PeerManager::removeAllPeers()
{
   for (QListIterator<Peer*> i(this->peers); i.hasNext();)
      i.next()->setAsDead();
}

void PeerManager::newConnection(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      return;

   // Detach the socket to use it into a thread.
   tcpSocket->setParent(0);

   L_DEBU(QString("New pending socket from %1").arg(tcpSocket->peerAddress().toString()));
   if (!this->timer.isActive())
      this->timer.start();

   this->pendingSockets << PendingSocket(tcpSocket);

   connect(tcpSocket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(tcpSocket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   if (!tcpSocket->isValid())
   {
      L_DEBU("PeerManager::newConnection(..): socket isn't valid, disconnecting");
      this->disconnect(tcpSocket);
   }
   else
      this->dataReceived(tcpSocket); // The case where some data arrived before the 'connect' above.
}

void PeerManager::onGetChunk(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PeerMessageSocket> socket)
{
   if (this->receivers(SIGNAL(getChunk(QSharedPointer<FM::IChunk>, int, QSharedPointer<PM::ISocket>))) < 1)
   {
      Protos::Core::GetChunkResult mess;
      mess.set_status(Protos::Core::GetChunkResult_Status_ERROR_UNKNOWN);
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
      tcpSocket = dynamic_cast<QTcpSocket*>(this->sender());

   if (tcpSocket->bytesAvailable() >= Common::MessageHeader::HEADER_SIZE)
   {
      const Common::MessageHeader header = Common::MessageHeader::readHeader(*tcpSocket, false);
      Peer* p = this->getPeer_(header.getSenderID());

      this->removeFromPending(tcpSocket);
      disconnect(tcpSocket, SIGNAL(readyRead()), 0, 0);
      disconnect(tcpSocket, SIGNAL(disconnected()), 0, 0);

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
      tcpSocket = dynamic_cast<QTcpSocket*>(this->sender());

   this->removeFromPending(tcpSocket);
   tcpSocket->deleteLater();

   L_DEBU("Pending socket disconnected and deleted");
}

void PeerManager::checkIdlePendingSockets()
{
   for (QMutableListIterator<PendingSocket> i(this->pendingSockets); i.hasNext();)
   {
      PendingSocket pendingSocket = i.next();
      if (static_cast<quint32>(pendingSocket.t.elapsed()) > SETTINGS.get<quint32>("pending_socket_timeout"))
      {
         i.remove();
         // Without these 'disconnect' this warning is printed by Qt : "QCoreApplication::postEvent: Unexpected null receiver".
         disconnect(pendingSocket.socket, SIGNAL(readyRead()), 0, 0);
         disconnect(pendingSocket.socket, SIGNAL(disconnected()), 0, 0);
         pendingSocket.socket->deleteLater();
         L_DEBU("Pending socket timeout -> deleted");
      }
   }

   if (this->pendingSockets.isEmpty())
      this->timer.stop();
}

void PeerManager::peerUnbanned()
{
   Peer* peer = static_cast<Peer*>(this->sender());
   if (peer->isAvailable())
      emit peerBecomesAvailable(peer);
}

void PeerManager::removeFromPending(QTcpSocket* socket)
{
   for (QMutableListIterator<PendingSocket> i(this->pendingSockets); i.hasNext();)
   {
      PendingSocket pendingSocket = i.next();
      if (pendingSocket.socket == socket)
      {
         i.remove();
         break;
      }
   }

   if (this->pendingSockets.isEmpty())
      this->timer.stop();
}
