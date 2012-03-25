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
  
#ifndef PEERMANAGER_PEER_H
#define PEERMANAGER_PEER_H

#include <QElapsedTimer>
#include <QTimer>
#include <QString>
#include <QTcpSocket>
#include <QHostAddress>
#include <QSharedPointer>
#include <QMutex>

#include <google/protobuf/text_format.h>

#include <Common/Hash.h>
#include <Common/Uncopyable.h>

#include <Core/FileManager/IGetHashesResult.h>
#include <Core/FileManager/IFileManager.h>

#include <IPeer.h>
#include <priv/ConnectionPool.h>

namespace PM
{   
   class Socket;
   class PeerManager;

   class Peer : public QObject, public IPeer, Common::Uncopyable
   {
      Q_OBJECT
      static const quint32 MAX_SPEED;

   public:
      Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID, const QString& nick = QString());

      QString toStringLog() const;

      Common::Hash getID() const;
      QHostAddress getIP() const;
      quint16 getPort() const;
      QString getNick() const;
      QString getCoreVersion() const;
      quint64 getSharingAmount() const;

      quint32 getSpeed();
      void setSpeed(quint32 newSpeed);

      void ban(int duration, const QString& reason = QString());

      bool isAlive() const;
      bool isAvailable() const;
      void update(
         const QHostAddress& IP,
         quint16 port,
         const QString& nick,
         const quint64& sharingAmount,
         const QString& coreVersion
      );

      QSharedPointer<IGetEntriesResult> getEntries(const Protos::Core::GetEntries& dirs);
      QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file);
      QSharedPointer<IGetChunkResult> getChunk(const Protos::Core::GetChunk& chunk);

      void newConnexion(QTcpSocket* tcpSocket);

   signals:
      void unbanned();

   private slots:
      void consideredDead();
      void unban();

   private:
      mutable QMutex mutex;

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      ConnectionPool connectionPool;

      Common::Hash ID;
      QHostAddress IP;
      quint16 port;
      QString nick;
      QString coreVersion;
      quint64 sharingAmount;

      QElapsedTimer speedTimer;
      quint32 speed; // [bytes/s]

      bool alive;
      QTimer aliveTimer;

      bool banned;
      QString bannedReason;
      QTimer bannedTimer;

      quint32 averageSpeed;
      QDateTime lastUpdateAverageSpeed;
   };
}
#endif
