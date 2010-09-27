#include <PeerUpdater.h>

#include <Common/Hash.h>
#include <Constants.h>
#include <priv/PeerManager.h>

PeerUpdater::PeerUpdater(QList< QSharedPointer<FM::IFileManager> > fileManagers, QList< QSharedPointer<IPeerManager> > peerManagers)
   : fileManagers(fileManagers), peerManagers(peerManagers)
{
   this->timer.setInterval(1000);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(update()));

   for (int i = 0; i < this->peerManagers.size(); i++)
   {
      dynamic_cast<PeerManager*>(this->peerManagers[i].data())->setID(Common::Hash::rand());
      this->peerManagers[i]->setNick("Bob " + QString::number(i + 1));
   }
}

void PeerUpdater::start()
{
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
               PORT + j,
               this->peerManagers[j]->getNick(),
               this->fileManagers[j]->getAmount()
            );
      }
   }
}
