#include <priv/PeerManager.h>
using namespace PM;

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/Network.h>
#include <Common/PersistantData.h>

#include <Priv/Log.h>
#include <Priv/Constants.h>

/**
  * @class PeerManager
  *
  */

PeerManager::PeerManager(QSharedPointer<FM::IFileManager> fileManager)
   : fileManager(fileManager)
{
   L_USER("Loading ..");

   Protos::Common::Settings settings;

   try
   {
      Common::PersistantData::getValue(FILE_SETTINGS, settings);
      this->ID = settings.peerid().hash().data();
      this->nick = settings.nick().data();
   }
   catch (Common::UnknownValueException)
   {
      this->ID = Common::Hash::rand();
      this->nick = "Bob";
      settings.mutable_peerid()->set_hash(this->ID.getData(), Common::Hash::HASH_SIZE);
      settings.set_nick(this->nick.toStdString());
      Common::PersistantData::setValue(FILE_SETTINGS, settings);
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

    Protos::Common::Settings settings;
    Common::PersistantData::getValue(FILE_SETTINGS, settings);
    settings.set_nick(this->nick.toStdString());
    Common::PersistantData::setValue(FILE_SETTINGS, settings);
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
      peers << i.next();

   return peers;
}

/**
  * Return the IPeer* coresponding to ID in the peer list.
  * Return 0 is the peer doesn't exist.
  */
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

   this->pendingSockets << tcpSocket;
   connect(tcpSocket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(tcpSocket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   if (!tcpSocket->isValid())
      this->disconnect(tcpSocket);
   else
      this->dataReceived(tcpSocket); // The case where some data arrived before the 'connect' above.
}

void PeerManager::dataReceived(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      tcpSocket = dynamic_cast<QTcpSocket*>(this->sender());

   if (tcpSocket->bytesAvailable() >= Common::Network::HEADER_SIZE)
   {
      Common::MessageHeader header = Common::Network::readHeader(*tcpSocket, false);
      Peer* p = this->getPeer_(header.senderID);

      this->pendingSockets.removeOne(tcpSocket);
      disconnect(tcpSocket, SIGNAL(readyRead()));
      disconnect(tcpSocket, SIGNAL(disconnected()));

      if (p)
         p->newConnexion(tcpSocket);
   }
}

void PeerManager::disconnected(QTcpSocket* tcpSocket)
{
   if (!tcpSocket)
      tcpSocket = dynamic_cast<QTcpSocket*>(this->sender());

   this->pendingSockets.removeOne(tcpSocket);
   delete tcpSocket;
}
