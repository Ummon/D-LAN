#include <priv/PeerManager.h>
using namespace PM;

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/Network.h>
#include <Common/Settings.h>

#include <Priv/Log.h>
#include <Priv/Constants.h>

/**
  * @class PeerManager
  *
  */

PeerManager::PeerManager(QSharedPointer<FM::IFileManager> fileManager)
   : fileManager(fileManager)
{
   this->timer.setInterval(SETTINGS.get<quint32>("pending_socket_timeout") / 10);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(checkIdlePendingSockets()));

   Protos::Common::Settings settings;

   if (SETTINGS.arePersisted())
   {

#if DEBUG
   this->ID = Common::Hash::rand();
#else
   SETTINGS.get("peerID", this->ID);
#endif

      SETTINGS.get("nick", this->nick);
   }
   else
   {
      this->ID = Common::Hash::rand();
      this->nick = "Bob";
      SETTINGS.set("peerID", this->ID);
      SETTINGS.set("nick", this->nick);
      SETTINGS.save();
   }

   L_USER(QString("Our current ID: %1").arg(this->ID.toStr()));
}

Common::Hash PeerManager::getID()
{
   return this->ID;
}

/**
  * Set the current nick.
  */
void PeerManager::setNick(const QString& nick)
{
    this->nick = nick;
    SETTINGS.set("nick", this->nick);
    SETTINGS.save();
}

/**
  * Get the current nick.
  */
QString PeerManager::getNick()
{
   return this->nick;
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

   for (QListIterator<Peer*> i(this->peers); i.hasNext();)
   {
      Peer* peer = i.next();
      if (peer->getID() == ID)
         return peer;
   }

   return 0;
}

/**
  * A peer just send a IAmAlive packet, we update information about it
  */
void PeerManager::updatePeer(const Common::Hash& ID, const QHostAddress& IP, quint16 port, const QString& nick, const quint64& sharingAmount)
{
   if (ID.isNull() || ID == this->ID)
      return;

   L_DEBU(QString("%1 (%2) is alive!").arg(ID.toStr()).arg(nick));

   Peer* peer = this->getPeer_(ID);
   if (!peer)
   {
      peer = new Peer(this, this->fileManager, ID);
      this->peers << peer;
   }
   peer->update(IP, port, nick, sharingAmount);
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
      this->disconnect(tcpSocket);
   else
      this->dataReceived(tcpSocket); // The case where some data arrived before the 'connect' above.
}

void PeerManager::onGetChunk(Common::Hash hash, int offset, Socket* socket)
{
   if (this->receivers(SIGNAL(getChunk(Common::Hash, int, PM::ISocket*))) < 1)
   {
      Protos::Core::GetChunkResult mess;
      mess.set_status(Protos::Core::GetChunkResult_Status_ERROR_UNKNOWN);
      socket->send(0x52, mess);
      socket->finished();
      L_ERRO("PeerManager::onGetChunk(..) : no slot connected to the signal 'getChunk(..)'");
      return;
   }

   emit getChunk(hash, offset, socket);
}

void PeerManager::dataReceived(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      tcpSocket = dynamic_cast<QTcpSocket*>(this->sender());

   if (tcpSocket->bytesAvailable() >= Common::Network::HEADER_SIZE)
   {
      const Common::MessageHeader& header = Common::Network::readHeader(*tcpSocket, false);
      Peer* p = this->getPeer_(header.senderID);

      this->removeFromPending(tcpSocket);
      disconnect(tcpSocket, SIGNAL(readyRead()), 0, 0);
      disconnect(tcpSocket, SIGNAL(disconnected()), 0, 0);

      if (p)
         p->newConnexion(tcpSocket);
      else
         this->disconnected(tcpSocket);
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
