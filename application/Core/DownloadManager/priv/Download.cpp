#include <priv/Download.h>
using namespace DM;

#include <priv/Constants.h>
#include <priv/Log.h>

quint64 Download::currentID(1);

Download::Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : ID(currentID++), fileManager(fileManager), peerManager(peerManager), peerSourceID(peerSourceID), peerSource(0), entry(entry), status(QUEUED)
{
   this->retrievePeer();
}

Download::~Download()
{
   emit deleted(this);
}

void Download::populateEntry(Protos::Queue::Queue_Entry* entry) const
{
   entry->mutable_entry()->CopyFrom(this->entry);
   entry->mutable_peer_id()->set_hash(this->peerSourceID.getData(), Common::Hash::HASH_SIZE);
   entry->set_complete(this->status == COMPLETE);
}

quint64 Download::getID() const
{
   return this->ID;
}

Status Download::getStatus() const
{
   return this->status;
}

bool Download::isStatusErroneous() const
{
   return this->status >= 0x20;
}

int Download::getProgress() const
{
   return 0;
}

Common::Hash Download::getPeerSourceID() const
{
   return this->peerSourceID;
}

const Protos::Common::Entry& Download::getEntry()
{
   return this->entry;
}

void Download::remove()
{
   delete this;
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
      L_DEBU(QString("Unable to retrieve the peer, peerID = %1").arg(this->peerSourceID.toStr()));
      this->status = UNKNOWN_PEER;
      QTimer::singleShot(CHECK_DEAD_PEER_PERIOD, this, SLOT(retrievePeer()));
   }
   else
   {
      this->status = QUEUED;
   }
}

