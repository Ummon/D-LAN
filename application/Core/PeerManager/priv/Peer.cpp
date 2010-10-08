#include <priv/Peer.h>
using namespace PM;

#include <Common/Settings.h>
#include <Common/LogManager/Builder.h>

#include <priv/PeerManager.h>
#include <priv/Constants.h>
#include <priv/Log.h>
#include <priv/GetEntriesResult.h>
#include <priv/GetHashesResult.h>
#include <priv/GetChunkResult.h>

const quint32 Peer::MAX_SPEED = 4294967295u;

Peer::Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID)
   : peerManager(peerManager), fileManager(fileManager), connectionPool(peerManager, fileManager, ID), ID(ID), speed(MAX_SPEED), alive(false)
{
   this->aliveTimer.setSingleShot(true);
   this->aliveTimer.setInterval(SETTINGS.getUInt32("peer_timeout_factor") * SETTINGS.getUInt32("peer_imalive_period"));
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

quint32 Peer::getSpeed()
{
   if (static_cast<quint32>(this->lastSpeedUpdate.restart()) > 1000 * SETTINGS.getUInt32("download_rate_valid_time_factor") / (SETTINGS.getUInt32("lan_speed") / 1024 / 1024))
      this->speed = MAX_SPEED;
   return this->speed;
}

void Peer::setSpeed(quint32 speed)
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

