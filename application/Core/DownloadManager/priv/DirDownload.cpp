#include <priv/DirDownload.h>
using namespace DM;

#include <Common/ProtoHelper.h>

#include <priv/Log.h>

DirDownload::DirDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry)
{
   L_DEBU(QString("New DirDownload : path = %1/%2 source = %3").
      arg(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::path)).
      arg(Common::ProtoHelper::getStr(entry, &Protos::Common::Entry::name)).
      arg(this->peerSourceID.toStr())
   );
}

QList<Common::Hash> DirDownload::getPeers() const
{
   return QList<Common::Hash>() << this->peerSourceID;
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
   this->getEntriesResult.clear(); // Is the 'IGetEntriesResult' object is deleted? Must we disconnect the signal? Answer : No the signal is automatically disconnected.

   // We need to specify the shared directory for each entry.
   Protos::Core::GetEntriesResult entriesCopy(entries);
   for (int i = 0; i < entries.entry_size(); i++)
      entriesCopy.mutable_entry(i)->mutable_shared_dir()->CopyFrom(this->entry.shared_dir());

   emit newEntries(entriesCopy);
}

