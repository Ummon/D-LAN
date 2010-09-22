#include <priv/Peer.h>
using namespace PM;

#include <Common/LogManager/Builder.h>

#include <priv/PeerManager.h>
#include <priv/Constants.h>
#include <priv/Log.h>

Peer::Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID)
   : peerManager(peerManager), fileManager(fileManager), ID(ID), alive(false)
{
   this->aliveTimer.setSingleShot(true);
   this->aliveTimer.setInterval(PEER_TIMEOUT * 1000);
   connect(&this->aliveTimer, SIGNAL(timeout()), this, SLOT(consideredDead()));

   connect(&this->connectionPool, SIGNAL(newMessage(quint32, google::protobuf::Message, Socket*)), this, SLOT(messageReceived(quint32, google::protobuf::Message, Socket*)));
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

void Peer::update(const QHostAddress&  IP, quint16 port, const QString& nick, const quint64& sharingAmount)
{
   this->alive = true;
   this->aliveTimer.start();

   this->IP = IP;
   this->port = port;
   this->nick = nick;
   this->sharingAmount = sharingAmount;

   this->connectionPool.setIP(this->IP, this->port);
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

void Peer::getEntries(const Protos::Common::Entry& dir)
{
   Protos::Core::GetEntries entriesMessage;
   entriesMessage.mutable_dir()->CopyFrom(dir);
}


void Peer::getHashes(const Protos::Common::FileEntry& file)
{

}

void Peer::getChunk(const Protos::Core::GetChunk& chunk)
{

}

void Peer::newConnexion(QTcpSocket* tcpSocket)
{
   L_DEBU(QString("New Connection from %1").arg(this->toStr()));
   this->connectionPool.newConnexion(tcpSocket);
}

void Peer::messageReceived(quint32 type, const google::protobuf::Message& message, Socket* socket)
{
   switch (type)
   {
   case 0x31 :
      const Protos::Core::GetEntries& getEntriesMessage = dynamic_cast<const Protos::Core::GetEntries&>(message);
      socket->send(
         0x32,
         getEntriesMessage.has_dir() ? this->fileManager->getEntries(getEntriesMessage.dir()) : this->fileManager->getEntries(),
         this->peerManager->getID()
      );
      socket->finished();
//   case 0x32 :

//   case 0x41 :
//   case 0x42 :
//   case 0x43 :
//   case 0x44 :

//   case 0x51 :
//   case 0x52 :
   }
}

/**
  * Mainly for debugging purpose.
  */
QString Peer::toStr()
{
   return QString("%1 - %2 - %3").arg(this->nick).arg(this->ID.toStr()).arg(this->IP.toString());
}

//void Peer::messageReceived(Socket* socket, const google::protobuf::Message& message)
//{

//}

void Peer::consideredDead()
{
   L_DEBU(QString("Peer \"%1\" is dead").arg(this->nick));
   this->alive = false;
}
