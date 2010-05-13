#include <priv/Peer.h>
using namespace PM;

#include <Common/LogManager/Builder.h>

const int Peer::port = 55142;

/**
 * Constructor: Create a new peer, based on his ID
 * @author mcuony
 */
::Peer::Peer(Common::Hash ID) : ID(ID)
{
   this->logger = LM::Builder::newLogger("PeerManager::Peer[" + this->ID.toStr() + "]");
   QTcpSocket nsocket;
   this->socket = QSharedPointer<QTcpSocket>(&nsocket);
}

/**
 * Set the lastUpdate to now
 *
 * @author mcuony
 */
void ::Peer::justSeen( const QHostAddress&  peerIP, const QString& peerNick, const quint64& peerAmount)
{
   this->IisAlive = true;
   this->lastUpdate =  QDateTime::currentDateTime();
   this->IP = peerIP;
   this->nick = peerNick;
   this->amount = peerAmount;

   this->send("Coucou");
}

/**
 * Test if the peer is dead or not, and set IisAlive
 *
 * @author mcuony
 */
bool ::Peer::haveYouToDie()
{
   int nSec = this->lastUpdate.secsTo(QDateTime::currentDateTime()) ;

   if (nSec > Peer::ttl)
   {
      this->IisAlive = false;
      return true;
   }
   return false;
}

/**
 * Return true if the peer is alive
 *
 * @author mcuony
 */
bool ::Peer::isAlive()
{
   return this->IisAlive;
}

/**
 * Return the id of the peer
 *
 * @author mcuony
 */

Common::Hash Peer::getId()
{
   return this->ID;
}

bool ::Peer::send(const QByteArray& data)
{
   if (this->IisAlive == false)
   {
      return false;
   }

   if (this->socket->isOpen() == false) {
      QObject::connect(this->socket.data(), SIGNAL(connected()),this,SLOT(connected()));
      QObject::connect(this->socket.data(), SIGNAL(readyRead()), this, SLOT(gotData()));
      this->socket->connectToHost(this->IP, Peer::port);

      LOG_DEBU(this->logger, "Someone want to send data to the peer, but not connected yet, connecting...");

      this->bufferToWrite.append(data);
   }
   else
      this->socket->write(data);

   return true;

}
Common::Hashes* ::Peer::getHashes(const Protos::Common::FileEntry& file)
{
}
IGetEntries* ::Peer::getEntries(const Protos::Common::DirEntry& dir)
{
}
void ::Peer::receive(QByteArray& data)
{
}

void ::Peer::connected()
{

   LOG_DEBU(this->logger, "Now connected to the peer as requested");

   if (this->bufferToWrite.length() > 0) {
      this->socket->write(this->bufferToWrite);
      this->bufferToWrite.clear();
      LOG_DEBU(this->logger, "Some data was waiting for the peer. Flushed.");
   }
}

void ::Peer::gotData()
{
   while (this->socket->canReadLine())
   {
      LOG_DEBU(this->logger, "Data:" + this->socket->readLine());
   }
}

QHostAddress Peer::Peer::getIp()
{
   return this->IP;
}

void ::Peer::newSocket(QSharedPointer<QTcpSocket> newSocket)
{

     this->socket = newSocket;

     QObject::connect(this->socket.data(),SIGNAL(connected()),this,SLOT(connected()));
     QObject::connect(this->socket.data(), SIGNAL(readyRead()), this, SLOT(gotData()));
}
