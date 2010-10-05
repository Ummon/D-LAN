#include <priv/Peer.h>
using namespace PM;

#include <Common/LogManager/Builder.h>

#include <priv/PeerManager.h>
#include <priv/Constants.h>
#include <priv/Log.h>
#include <priv/GetEntriesResult.h>
#include <priv/GetHashesResult.h>
#include <priv/GetChunkResult.h>

Peer::Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID)
   : peerManager(peerManager), fileManager(fileManager), connectionPool(peerManager, fileManager, ID), ID(ID), speed(-1), alive(false)
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

int Peer::getSpeed()
{
   return this->speed;
}

void Peer::setSpeed(int speed)
{
   this->speed = speed;
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
   return QSharedPointer<IGetChunkResult>(
      new GetChunkResult(chunk, this->connectionPool.getASocket())
   );
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

void Peer::consideredDead()
{
   L_DEBU(QString("Peer \"%1\" is dead").arg(this->nick));
   this->connectionPool.closeAllSocket();
   this->alive = false;
}

