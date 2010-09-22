#include <PeerUpdater.h>

#include <Common/Hash.h>
#include <Constants.h>

PeerUpdater::PeerUpdater(QSharedPointer<IPeerManager> peerManager, int n)
   : peerManager(peerManager)
{
   this->timer.setInterval(200); // 200ms.
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(update()));
   this->timer.start();

   for (int i = 0; i < n; i++)
   {
      PeerData peer;
      peer.ID = Common::Hash::rand();
      peer.IP = QHostAddress::LocalHost;
      peer.nick = "Bob" + QString::number(i);
      peer.sharingAmount = 42 + 10 * i;
      this->peers << peer;
   }
}

QList<PeerData> PeerUpdater::getPeers()
{
   return this->peers;
}

void PeerUpdater::stop()
{
   this->timer.stop();
}

void PeerUpdater::update()
{
   for (QListIterator<PeerData> i(this->peers); i.hasNext();)
   {
      PeerData peerData = i.next();
      this->peerManager->updatePeer(peerData.ID, peerData.IP, PORT, peerData.nick, peerData.sharingAmount);
   }
}
