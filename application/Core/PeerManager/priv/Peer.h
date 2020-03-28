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
  
#pragma once

#include <QElapsedTimer>
#include <QTimer>
#include <QString>
#include <QTcpSocket>
#include <QHostAddress>
#include <QSharedPointer>
#include <QMutex>

#include <google/protobuf/text_format.h>

#include <Common/Hash.h>
#include <Common/Constants.h>
#include <Common/Uncopyable.h>

#include <Core/FileManager/IGetHashesResult.h>
#include <Core/FileManager/IFileManager.h>

#include <IPeer.h>
#include <priv/ConnectionPool.h>

namespace PM
{   
   class PeerMessageSocket;
   class PeerManager;

   class Peer : public QObject, public IPeer, Common::Uncopyable
   {
      Q_OBJECT
      static const quint32 MAX_SPEED;

   public:
      Peer(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, Common::Hash ID, const QString& nick = QString());

      virtual QString toStringLog() const;

      virtual Common::Hash getID() const;
      virtual QHostAddress getIP() const;
      virtual quint16 getPort() const;
      virtual QString getNick() const;
      virtual QString getCoreVersion() const;
      virtual quint64 getSharingAmount() const;
      virtual quint32 getDownloadRate() const;
      virtual quint32 getUploadRate() const;

      virtual quint32 getSpeed();
      virtual void setSpeed(quint32 newSpeed);

      virtual void block(int duration, const QString& reason = QString());

      virtual bool isAlive() const;
      virtual bool isAvailable() const;
      virtual quint32 getProtocolVersion() const;
      virtual void update(
         const QHostAddress& IP,
         quint16 port,
         const QString& nick,
         const quint64& sharingAmount,
         const QString& coreVersion,
         quint32 downloadRate,
         quint32 uploadRate,
         quint32 protocolVersion
      );
      virtual void setAsDead();

      virtual QSharedPointer<IGetEntriesResult> getEntries(const Protos::Core::GetEntries& dirs);
      virtual QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file);
      virtual QSharedPointer<IGetChunkResult> getChunk(const Protos::Core::GetChunk& chunk);

      void newConnexion(QTcpSocket* tcpSocket);

   signals:
      void unblocked();

   protected slots:
      void consideredDead();
      void unblock();

   protected:
      bool isVersionCompatible() const { return this->protocolVersion == Common::Constants::PROTOCOL_VERSION; }

      mutable QMutex mutex;

      ConnectionPool connectionPool;

      Common::Hash ID;
      QHostAddress IP;
      quint16 port;
      QString nick;
      QString coreVersion;
      quint64 sharingAmount;
      quint32 downloadRate;
      quint32 uploadRate;

      QElapsedTimer speedTimer;
      quint32 speed; // [bytes/s]

      bool alive;
      QTimer aliveTimer;

      bool blocked;
      QString blockedReason;
      QTimer blockedTimer;

      quint32 protocolVersion;
   };
}
