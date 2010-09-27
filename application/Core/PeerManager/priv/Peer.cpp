#include <priv/Peer.h>
using namespace PM;

#include <Common/LogManager/Builder.h>

#include <priv/PeerManager.h>
#include <priv/Constants.h>
#include <priv/Log.h>
#include <priv/GetEntriesResult.h>
#include <priv/GetHashesResult.h>

Peer::Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID)
   : peerManager(peerManager), fileManager(fileManager), connectionPool(peerManager, fileManager), ID(ID), alive(false)
{
   this->aliveTimer.setSingleShot(true);
   this->aliveTimer.setInterval(PEER_TIMEOUT * 1000);
   connect(&this->aliveTimer, SIGNAL(timeout()), this, SLOT(consideredDead()));

//   connect(&this->connectionPool, SIGNAL(newMessage(quint32, google::protobuf::Message, Socket*)), this, SLOT(messageReceived(quint32, google::protobuf::Message, Socket*)));
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

QSharedPointer<IGetEntriesResult> Peer::getEntries(const Protos::Core::GetEntries& dir)
{
   return QSharedPointer<IGetEntriesResult>(
      new GetEntriesResult(dir, this->connectionPool.getASocket())
   );
}


QSharedPointer<IGetHashesResult> Peer::getHashes(const Protos::Common::Entry& file)
{
   return QSharedPointer<IGetHashesResult>(
      new GetHashesResult(file, this->connectionPool.getASocket())
   );
}

QSharedPointer<IGetChunkResult> Peer::getChunk(const Protos::Core::GetChunk& chunk)
{
   return QSharedPointer<IGetChunkResult>();
}

void Peer::newConnexion(QTcpSocket* tcpSocket)
{
   L_DEBU(QString("New Connection from %1").arg(this->toStr()));
   this->connectionPool.newConnexion(tcpSocket);
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

