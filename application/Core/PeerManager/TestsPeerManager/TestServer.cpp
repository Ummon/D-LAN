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
  
#include <TestServer.h>

#include <QSharedPointer>
#include <QtDebug>
#include <QTest>

#include <Common/Constants.h>

/**
  * @class TestServer
  *
  * Listen for new connection and forward them to the given peerManager.
  * This class is also used by /Core/DownloadManager/tests
  */

TestServer::TestServer(QSharedPointer<PM::IPeerManager> peerManager, int port) :
   peerManager(peerManager)
{
   connect(&this->server, SIGNAL(newConnection()), this, SLOT(newConnection()));
   QVERIFY(this->server.listen(QHostAddress::Any, port));
}

void TestServer::newConnection()
{
   qDebug() << "TestServer::newConnection()";
   QTcpSocket* socket = this->server.nextPendingConnection();
   connect(socket, SIGNAL(disconnected()), socket, SLOT(deleteLater()));
   this->peerManager->newConnection(socket);
}
