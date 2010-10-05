#include <priv/DirDownload.h>
using namespace DM;

DirDownload::DirDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry)
{
}

/**
  * Ask the DirDownload to get its content.
  * The signal 'newEntries' will be emitted when the answer is received.
  */
void DirDownload::retrieveEntries()
{
   if (!this->hasAValidPeer())
      return;

   Protos::Core::GetEntries getEntries;
   getEntries.mutable_dir()->CopyFrom(this->entry);
   this->getEntriesResult = this->peerSource->getEntries(getEntries);
   connect(this->getEntriesResult.data(), SIGNAL(result(Protos::Core::GetEntriesResult)), this, SLOT(result(Protos::Core::GetEntriesResult)));
   this->getEntriesResult->start();
}

void DirDownload::result(const Protos::Core::GetEntriesResult& entries)
{
   this->getEntriesResult.clear(); // TODO : is the 'IGetEntriesResult' object is deleted? Must we disconnect the signal?
   emit newEntries(entries);
}

