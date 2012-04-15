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
  
#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QtNetwork>
#include <QList>
#include <QDateTime>
#include <QSharedPointer>

#include <Common/Uncopyable.h>
#include <Common/Network/MessageSocket.h>

#include <Core/FileManager/IFileManager.h>
#include <Core/FileManager/IChunk.h>

#include <priv/PeerMessageSocket.h>

namespace PM
{
   class PeerMessageSocket;
   class PeerManager;

   class ConnectionPool : public QObject, Common::Uncopyable
   {
      Q_OBJECT

   public:
      ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID);
      ~ConnectionPool();

      void setIP(const QHostAddress& IP, quint16 port);
      void newConnexion(QTcpSocket* socket);

      QSharedPointer<PeerMessageSocket> getASocket();
      void closeAllSocket();

   private slots:
      void socketBecomeIdle(PeerMessageSocket* socket);
      void socketClosed(PeerMessageSocket* socket);
      void socketGetChunk(QSharedPointer<FM::IChunk> chunk, int offset, PeerMessageSocket* socket);

   private:
      enum Direction { TO_PEER, FROM_PEER };
      QSharedPointer<PeerMessageSocket> addNewSocket(QSharedPointer<PeerMessageSocket> socket, Direction direction);
      QList< QSharedPointer<PeerMessageSocket> > getAllSockets() const;

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      QList< QSharedPointer<PeerMessageSocket> > socketsToPeer;
      QList< QSharedPointer<PeerMessageSocket> > socketsFromPeer;

      QHostAddress peerIP;
      quint16 port;
      const Common::Hash peerID;
   };
}

#endif
