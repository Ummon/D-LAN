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
  
#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QtNetwork>
#include <QList>
#include <QDateTime>
#include <QSharedPointer>

#include <Common/Uncopyable.h>

#include <Core/FileManager/IFileManager.h>

#include <priv/Socket.h>

namespace PM
{
   class Socket;
   class PeerManager;

   class ConnectionPool : public QObject, Common::Uncopyable
   {
      Q_OBJECT

   public:
      ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID);
      ~ConnectionPool();

      void setIP(const QHostAddress& IP, quint16 port);
      void newConnexion(QTcpSocket* socket);

      QSharedPointer<Socket> getASocket();
      void closeAllSocket();

   private slots:
      void socketGetIdle(Socket* socket);
      void socketClosed(Socket* socket);
      void socketGetChunk(const Common::Hash& hash, int offset, Socket* socket);

   private:
      QSharedPointer<Socket> addNewSocket(QSharedPointer<Socket> socket);

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      QList< QSharedPointer<Socket> > sockets;
      QHostAddress peerIP;
      quint16 port;
      const Common::Hash peerID;
   };
}

#endif
