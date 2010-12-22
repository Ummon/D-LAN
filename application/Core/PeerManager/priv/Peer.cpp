/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
   this->aliveTimer.setInterval(SETTINGS.get<quint32>("peer_timeout_factor") * SETTINGS.get<quint32>("peer_imalive_period"));
   connect(&this->aliveTimer, SIGNAL(timeout()), this, SLOT(consideredDead()));
}

QString Peer::toStringLog() const
{
   return QString("%1 %2 %3:%4 %5").arg(this->nick).arg(this->ID.toStr()).arg(this->IP.toString()).arg(this->port).arg(this->alive ? "<alive>" : "<dead>");
}

Common::Hash Peer::getID() const
{
   return this->ID;
}

QHostAddress Peer::getIP() const
{
   return this->IP;
}

quint16 Peer::getPort() const
{
   return this->port;
}

QString Peer::getNick() const
{
   return this->nick;
}

quint64 Peer::getSharingAmount() const
{
   return this->sharingAmount;
}

quint32 Peer::getSpeed()
{
   if (static_cast<quint32>(this->lastSpeedUpdate.restart()) > 1000 * SETTINGS.get<quint32>("download_rate_valid_time_factor") / (SETTINGS.get<quint32>("lan_speed") / 1024 / 1024))
      this->speed = MAX_SPEED;
   return this->speed;
}

void Peer::setSpeed(quint32 speed)
{
   if (speed == MAX_SPEED)
      this->speed = speed;
   else
      this->speed = (this->speed + speed) / 2;
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

QSharedPointer<IGetEntriesResult> Peer::getEntries(const Protos::Core::GetEntries& dirs)
{
   return QSharedPointer<IGetEntriesResult>(
      new GetEntriesResult(dirs, this->connectionPool.getASocket())
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

