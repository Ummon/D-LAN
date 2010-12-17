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
  
#ifndef TESTS_PEERMANAGER_PEERUPDATER_H
#define TESTS_PEERMANAGER_PEERUPDATER_H

#include <QSharedPointer>
#include <QTimer>
#include <QList>

#include <Core/FileManager/IFileManager.h>

#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IPeer.h>

struct PeerData
{
   Common::Hash ID;
   QHostAddress IP;
   int port;
   QString nick;
   quint64 sharingAmount;
};

class PeerUpdater : public QObject
{
   Q_OBJECT
public:
   PeerUpdater(QList< QSharedPointer<FM::IFileManager> > fileManagers, QList< QSharedPointer<PM::IPeerManager> > peerManagers, int port);

   void start();
   void stop();

private slots:
   void update();

private:
   QList< QSharedPointer<FM::IFileManager> > fileManagers;
   QList< QSharedPointer<PM::IPeerManager> > peerManagers;
   QTimer timer;
   const int port;
};

#endif
