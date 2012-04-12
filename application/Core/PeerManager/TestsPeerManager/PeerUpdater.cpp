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
  
#include <PeerUpdater.h>

#include <QtDebug>

#include <Common/Hash.h>
#include <Core/PeerManager/priv/PeerManager.h>

/**
  * @class PeerUpdater
  *
  * Simulate a periodic update, each peerManager will know the information of each other.
  * This class is also used by /Core/DownloadManager/tests
  */

PeerUpdater::PeerUpdater(QList< QSharedPointer<FM::IFileManager> > fileManagers, QList< QSharedPointer<PM::IPeerManager> > peerManagers, int port) :
   fileManagers(fileManagers), peerManagers(peerManagers), port(port)
{
   this->timer.setInterval(1000);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(update()));

   for (int i = 0; i < this->peerManagers.size(); i++)
   {
      this->peerManagers[i]->setNick("Bob " + QString::number(i + 1));
      qDebug() << QString("Peer[%1] : %2 %3").arg(i).arg(this->peerManagers[i]->getSelf()->getNick()).arg(this->peerManagers[i]->getSelf()->getID().toStr());
   }
}

void PeerUpdater::start()
{
   this->update();
   this->timer.start();
}

void PeerUpdater::stop()
{
   this->timer.stop();
}

void PeerUpdater::update()
{
   for (int i = 0; i < this->peerManagers.size(); i++)
   {
      for (int j = 0; j < this->peerManagers.size(); j++)
      {
         if (this->peerManagers[i]->getSelf()->getID() != this->peerManagers[j]->getSelf()->getID())
            this->peerManagers[i]->updatePeer(
               this->peerManagers[j]->getSelf()->getID(),
               QHostAddress::LocalHost,
               this->port + j,
               this->peerManagers[j]->getSelf()->getNick(),
               this->fileManagers[j]->getAmount(),
               QString()
            );
      }
   }
}
