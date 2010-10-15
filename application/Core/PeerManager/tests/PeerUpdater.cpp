#include "PeerUpdater.h"

#include <QtDebug>

#include <Common/Hash.h>
#include <Core/PeerManager/priv/PeerManager.h>

/**
  * @class PeerUpdater
  * Simulate a periodic update, each peerManager will know the information of each other.
  * This class is also used by /Core/DownloadManager/tests
  */

PeerUpdater::PeerUpdater(QList< QSharedPointer<FM::IFileManager> > fileManagers, QList< QSharedPointer<PM::IPeerManager> > peerManagers, int port)
   : fileManagers(fileManagers), peerManagers(peerManagers), port(port)
{
   this->timer.setInterval(1000);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(update()));

   for (int i = 0; i < this->peerManagers.size(); i++)
   {
      this->peerManagers[i]->setNick("Bob " + QString::number(i + 1));
      qDebug() << QString("Peer[%1] : %2 %3").arg(i).arg(this->peerManagers[i]->getNick()).arg(this->peerManagers[i]->getID().toStr());
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
         if (this->peerManagers[i]->getID() != this->peerManagers[j]->getID())
            this->peerManagers[i]->updatePeer(
               this->peerManagers[j]->getID(),
               QHostAddress::LocalHost,
               this->port + j,
               this->peerManagers[j]->getNick(),
               this->fileManagers[j]->getAmount()
            );
      }
   }
}
