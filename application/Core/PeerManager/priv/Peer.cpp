#include <priv/Peer.h>
using namespace PM;

#include <Common/LogManager/Builder.h>

#include <priv/Constants.h>
#include <priv/Log.h>

//const int Peer::port = 55142;

Peer::Peer(QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID)
   : fileManager(fileManager), ID(ID), alive(false)
{
   this->aliveTimer.setSingleShot(true);
   this->aliveTimer.setInterval(PEER_TIMEOUT * 1000);
   connect(&this->aliveTimer, SIGNAL(timeout()), this, SLOT(consideredDead()));
}

Common::Hash Peer::getID()
{
   return this->ID;
}

QHostAddress Peer::getIP()
{
   return this->IP;
}

QString Peer::getNick()
{
   return this->nick;
}

quint64 Peer::getSharingAmount()
{
   return this->sharingAmount;
}

bool Peer::isAlive()
{
   return this->alive;
}

/**
 * Set the lastUpdate to now
 */
void Peer::update(const QHostAddress&  IP, const QString& nick, const quint64& sharingAmount)
{
   this->alive = true;
   this->aliveTimer.start();

   this->IP = IP;
   this->nick = nick;
   this->sharingAmount = sharingAmount;
}

/*bool Peer::send(const QByteArray& data)
{
   if (this->IisAlive == false)
   {
      return false;
   }


   if (this->socket->isOpen() == false) {
      QObject::connect(this->socket.data(), SIGNAL(connected()),this,SLOT(connected()));
      QObject::connect(this->socket.data(), SIGNAL(readyRead()), this, SLOT(gotData()));
      this->socket->connectToHost(this->IP, Peer::port);

      this->bufferToWrite.append(data);
   }
   else
      this->socket->write(data);

   return true;
}*/

void Peer::getHashes(const Protos::Common::FileEntry& file)
{

}

void Peer::getEntries(const Protos::Common::Entry& dir)
{
   Protos::Core::GetEntries entriesMessage;
   entriesMessage.mutable_dir()->CopyFrom(dir);

   //this->connectionPool.getSocket(GET_ENTRIES)->write();
}

/*void Peer::receive(QByteArray& data)
{
}*/

void Peer::newConnexion(Common::MessageHeader header, QSharedPointer<QTcpSocket> socket)
{
   this->sockets << socket;

   connect(socket.data(), SIGNAL(stateChanged()), this, SLOT(stateChanged()));
   connect(socket.data(), SIGNAL(readyRead()), this, SLOT(dataReceived()));
}

void Peer::consideredDead()
{
   L_DEBU(QString("Peer \"%1\" is dead").arg(this->nick));
   this->alive = false;
}

void Peer::stateChanged(QAbstractSocket::SocketState socketState)
{
   /*
   LOG_DEBU(this->logger, "Now connected to the peer as requested");

   if (this->bufferToWrite.length() > 0)
   {
      this->socket->write(this->bufferToWrite);
      this->bufferToWrite.clear();
      LOG_DEBU(this->logger, "Some data was waiting for the peer. Flushed.");
   }*/
}

void Peer::dataReceived()
{
   /*
   while (this->socket->canReadLine())
   {
      LOG_DEBU(this->logger, "Data:" + this->socket->readLine());
   }*/
}
