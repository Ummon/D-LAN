#include <priv/Download.h>
using namespace DM;

Download::Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : fileManager(fileManager), peerManager(peerManager), peerSourceID(peerSourceID), peerSource(0), entry(entry), status(QUEUED), nbHashes(0)
{
   this->retrievePeer();
}

void Download::retrievePeer()
{
   this->peerSource = this->peerManager->getPeer(this->peerSourceID);
   if (!this->hasAValidPeer())
      this->status = NO_SOURCE;
}

bool Download::hasAValidPeer()
{
   return this->peerSource && this->peerSource->isAlive();
}


void Download::retreiveHashes()
{
   if (!this->hasAValidPeer())
      return;

   this->getHashesResult = this->peerSource->getHashes(this->entry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   this->getHashesResult->start();
}

void Download::result(const Protos::Core::GetHashesResult& result)
{
   if (result.status() == Protos::Core::GetHashesResult_Status_OK)
   {
      this->status = INITIALIZING;
      this->nbHashes = result.nb_hash();
   }
   else
   {
      this->status = NOT_FOUND;
      this->getHashesResult.clear();
   }
}

void Download::nextHash(const Common::Hash& hash)
{

}
