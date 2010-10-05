#include <priv/Download.h>
using namespace DM;

#include <priv/Constants.h>

Download::Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : fileManager(fileManager), peerManager(peerManager), peerSourceID(peerSourceID), peerSource(0), entry(entry), status(QUEUED), nbHashes(0)
{
   this->timer.setInterval(CHECK_DEAD_PEER_PERIOD);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(retrievePeer()));

   this->retrievePeer();
}

int Download::getId()
{
   // TODO
   return 0;
}

Status Download::getStatus()
{
   // TODO
   return DM::QUEUED;
}

char Download::getProgress()
{
   // TODO
   return 0;
}

Common::Hash Download::getPeerSourceID()
{
   return this->peerSourceID;
}

Protos::Common::Entry Download::getEntry()
{
   return this->entry;
}

void Download::remove()
{
   // TODO
}

bool Download::hasAValidPeer()
{
   return this->peerSource && this->peerSource->isAlive();
}

void Download::retrievePeer()
{
   this->peerSource = this->peerManager->getPeer(this->peerSourceID);
   if (!this->hasAValidPeer())
   {
      this->status |= UNKNOWN_PEER;
      this->timer.start();
   }
   else
   {
      this->status &= !UNKNOWN_PEER;
      this->timer.stop();
   }
}

